%%% vim: set ts=4 sts=4 sw=4 expandtab:
-module(fbi_sdb).

-behavior(gen_server).

%%% API
-export([
    cfg/1,
    client_stats/1,
    metric_stats/6,
    metric_stats/7,
    metrics_by_key/3,
    get_flags_map_and_register/2,
    reload_config/1,    % Reconfigure on the fly
    start_link/1,
    flags/2,
    flag/3,
    sdb_query/2,
    sdb_query/3
]).

%%% gen_server callbacks
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2
]).

%%% selftests

-record(state, {
    port,
    flags_map,
    flags_map_blob,
    clients,
    realm
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

client_stats(Realm) ->
    [{{rate, Rate}, IP} ||
        {IP, [{rate, Rate}|_]} <- lists:reverse(lists:sort(fun ({_, [{rate, R1}|_]}, {_, [{rate, R2}|_]}) -> R1 < R2 end,
                                                gen_server:call(fbi_scfg:sdb_proc(Realm), {sdb_query, client_stats})))].

metric_stats(Realm, Pattern, Limit, SortByCategory, AllCategories, Period) ->
    metric_stats(Realm, Pattern, Limit, SortByCategory, Period, AllCategories, 5000).
metric_stats(Realm, Pattern, Limit, SortByCategory, AllCategories, Period, Timeout) ->
    sdb_query(Realm, {groupped_metric_stats, Pattern, Limit, SortByCategory, AllCategories, Period}, Timeout).

metrics_by_key(Realm, Key, Timeout) ->
    sdb_query(Realm, {metrics_by_key, Key}, Timeout).

reload_config(Realm) ->
    sdb_query(Realm, {reload_config, cfg(fbi_scfg:sdb_conf_filename(Realm))}).

get_flags_map_and_register(Realm, Pid) ->
    gen_server:call(fbi_scfg:sdb_proc(Realm), {get_flags_map_and_register, Pid}).

start_link(Realm) -> 
    gen_server:start_link({local, fbi_scfg:sdb_proc(Realm)}, ?MODULE, Realm, []).

flags(Realm, Key) ->
    fbi_sdb_cache:flags(fbi_scfg:sdb_tab(Realm), Key).
flag(Realm, Flag, Key) ->
    fbi_sdb_cache:flag(fbi_scfg:sdb_tab(Realm), Flag, Key).

% Use carefully! Can be CPU-intensive
sdb_query(Realm, Q) ->
    sdb_query(Realm, Q, 5000).

% Use carefully! Can be CPU-intensive
sdb_query(Realm, Q, Timeout) ->
    gen_server:call(fbi_scfg:sdb_proc(Realm), {sdb_query, Q}, Timeout).

cfg(Filename) ->
    {ok, Path} = application:get_env(fbi_sdb, conf_path),
    filename:join([Path, Filename]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Realm) ->
    {_Host, UdpPort} = fbi_scfg:server_hostport(Realm),
    PortCmd = code:priv_dir(fbi_sdb) ++ "/fbi-sdb "
                ++ " --port "
                ++ integer_to_list(UdpPort)
                ++ " --lru "
                ++ integer_to_list(fbi_scfg:sdb_lru_size(Realm))
                ++ " " ++ fbi_scfg:raw_sdb_params(Realm)
                ++ " --gcso 800 --config "
                ++ cfg(fbi_scfg:sdb_conf_filename(Realm)),
    Port = open_port({spawn, PortCmd}, [{packet, 4}, binary, exit_status]),
    FlagsMap = fbi_sdb_cache:init_flags_map(fbi_scfg:sdb_tab(Realm)),
    {ok, #state{
            port = Port,
            flags_map = FlagsMap,
            flags_map_blob = undefined,
            clients = [],
            realm = Realm
        }}.

handle_call({get_flags_map_and_register, Pid}, _From, #state{flags_map_blob = FlagsMapBlob, clients = Clients, realm = Realm} = State) ->
    MRef = erlang:monitor(process, Pid),
    NewFlagsMapBlob = case FlagsMapBlob of
        undefined -> 
            Tab = fbi_scfg:sdb_tab(Realm),
            FlagsMap = fbi_sdb_cache:get_flags_map(Tab),
            blob_of_flags_map(FlagsMap);
        _ -> FlagsMapBlob
    end,
    {reply, NewFlagsMapBlob, State#state{flags_map_blob = NewFlagsMapBlob, clients = [{MRef, Pid} | Clients]}};
handle_call({sdb_query, Q}, From, #state{port = P} = State) ->
    port_command(P, term_to_binary({{from, From}, Q})),
    {noreply, State};
handle_call(status, _From, State) ->
    {reply, {ok, State}, State};
handle_call(Msg, _From, State) ->
    {reply, {error, {unknown_request, Msg}}, State}.

handle_info({P, {data,BinData}}, #state{port = P, clients = Clients, realm = Realm}=State) ->
    Data = binary_to_term(BinData),
    NewState = case dispatch(Realm, Data) of
        flag ->
            Blob = blob_of_data([Data]),
            [gen_server:cast(Pid, {data, Blob}) || {_MRef, Pid} <- Clients],
            State#state{flags_map_blob = undefined};
        direct_response -> State;
        _ ->
            lager:info("Unknown message from FBI SDB port: ~p", [Data]),
            State
    end,
    {noreply, NewState};
handle_info({P, {exit_status, _}}, #state{port=P}=State) ->
    {stop, exited, State};
handle_info({'DOWN', MRef, process, Pid, _Info},
                #state{clients = Clients} = State) ->
    NewClients = case lists:keytake(MRef, 1, Clients) of
        {value, {MRef, Pid}, NClients} -> NClients;
        _ -> Clients
    end,
    {noreply, State#state{clients = NewClients}};
handle_info(_Message, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

dispatch(Realm, {flagged, Flag, _HumanReadableLabel}) ->
    fbi_sdb_cache:add_flag(fbi_scfg:sdb_tab(Realm), Flag),
    flag;
dispatch(Realm, {unflagged, Flag, _HumanReadableLabel}) ->
    fbi_sdb_cache:delete_flag(fbi_scfg:sdb_tab(Realm), Flag),
    flag;
dispatch(_Realm, {{to, ReplyTo}, Data}) ->
    gen_server:reply(ReplyTo, Data),
    direct_response;
dispatch(_Realm, _Msg) ->
    error.

blob_of_flags_map(FlagsMap) ->
    blob_of_data([{flagged, Flag, ""} || {Flag, _} <- FlagsMap]).

blob_of_data([]) -> <<>>;
blob_of_data(Data) ->
    iolist_to_binary([
        begin
            case A of
                    flagged -> "+";
                    unflagged -> "-"
            end ++ string:join([binary_to_list(F), base64:encode_to_string(term_to_binary(L))], ";") ++ "\n"
        end || {A, F, L} <- Data]).
