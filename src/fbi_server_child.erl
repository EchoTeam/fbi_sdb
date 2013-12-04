%%% vim: set ts=4 sts=4 sw=4 expandtab:
-module(fbi_server_child).

-behavior(gen_server).

%%% API
-export([
    start_link/3
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

-record(state, {
    parent,
    parent_ref,
    sdb_ref,
    socket,
    realm
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Parent, Socket, Realm) ->
    gen_server:start_link(?MODULE, {Parent, Socket, Realm}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Parent, Socket, Realm}) ->
    ParentRef = erlang:monitor(process, Parent),
    SdbRef = erlang:monitor(process, fbi_scfg:sdb_proc(Realm)),
    {ok, #state{
            parent     = Parent,
            parent_ref = ParentRef,
            sdb_ref    = SdbRef,
            socket     = Socket,
            realm      = Realm
        }}.

handle_call(Msg, _From, State) ->
    {reply, {error, {unknown_request, Msg}}, State}.

handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State};
handle_info({'DOWN', ParentRef, process, Parent, _}, #state{parent = Parent, parent_ref = ParentRef} = State) ->
    {stop, normal, State};
handle_info({'DOWN', SdbRef, process, _, _}, #state{sdb_ref = SdbRef} = State) ->
    {stop, normal, State};
handle_info({tcp, Socket, "ping\n"}, #state{socket = Socket} = State) ->
    gen_tcp:send(Socket, "pong\n"),
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.

handle_cast(socket_ready, #state{socket = Socket, realm = Realm} = State) ->
    true = inet_db:register_socket(Socket, inet_tcp),
    inet:setopts(Socket, [{active, true}]),
    FlagsMap = fbi_sdb:get_flags_map_and_register(Realm, self()),
    send_data(Socket, FlagsMap),
    {noreply, State};
handle_cast({data, Data}, #state{socket = Socket} = State) ->
    send_data(Socket, Data),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_data(_Socket, <<>>) -> ok;
send_data(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
        ok -> ok;
        _ -> gen_tcp:close(Socket)
    end.

