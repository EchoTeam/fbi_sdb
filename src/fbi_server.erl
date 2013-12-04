%%% vim: set ts=4 sts=4 sw=4 expandtab:
-module(fbi_server).

-behavior(gen_server).

%%% API
-export([
    start_link/1
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
    socket,
    acceptor,
    clients,
    realm
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Realm) -> 
    gen_server:start_link({local, fbi_scfg:server_proc(Realm)}, ?MODULE, Realm, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Realm) ->
    process_flag(trap_exit, true),
    {_Host, TcpPort} = fbi_scfg:server_hostport(Realm),
    {ok, Socket} = gen_tcp:listen(TcpPort, [{active, false}, {packet, line}, {reuseaddr, true}]),
    {ok, Acceptor} = prim_inet:async_accept(Socket, -1),
    {ok, #state{
            socket = Socket,
            acceptor = Acceptor,
            clients = [],
            realm = Realm
        }}.

handle_call(Msg, _From, State) ->
    {reply, {error, {unknown_request, Msg}}, State}.

handle_info({inet_async, Socket, Acceptor, {ok, ClientSocket}},
        #state{socket = Socket, acceptor = Acceptor, clients = Clients, realm = Realm} = State) ->
    try
        {ok, Pid} = fbi_server_child:start_link(self(), ClientSocket, Realm),
        gen_tcp:controlling_process(ClientSocket, Pid),
        gen_server:cast(Pid, socket_ready),
        case prim_inet:async_accept(Socket, -1) of
            {ok, NewAcceptor} -> ok;
            {error, NewAcceptor} -> exit({async_accept, inet:format_error(NewAcceptor)})
        end,
        {noreply, State#state{acceptor = NewAcceptor, clients = [Pid | Clients]}}
    catch exit:R ->
        lager:info("Error in TCP acceptor ~p (~p)", [?MODULE, R]),
        {stop, R, State}
    end;
handle_info({inet_async, Socket, Acceptor, Error}, #state{socket = Socket, acceptor = Acceptor} = State) ->
    lager:info("Error in TCP acceptor ~p (~p)", [?MODULE, Error]),
    {stop, Error, State};
handle_info({'EXIT', Client, _}, #state{clients = Clients} = State) ->
    NewClients = lists:filter(fun(C) -> C /= Client end, Clients),
    {noreply, State#state{clients = NewClients}};
handle_info(_Message, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

