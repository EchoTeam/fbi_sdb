-module(fbi_sdb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, reconfigure/0, start_folsom_timer_server/1]).

%% Supervisor callbacks
-export([init/1]).

-define(FBI_STATS_INTERVAL, 60000).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, lists:concat([
            children_for_realm(Realm) || Realm <- fbi_scfg:enumerate()
        ])
    } }.

children_for_realm(Realm) -> [
        { fbi_utils:atom_for_realm(fbi_stats, Realm), {?MODULE, start_folsom_timer_server,[Realm]},
            permanent, 10000, worker, [folsom_timer_server]},
        { fbi_utils:atom_for_realm(fbi_sdb, Realm), { fbi_sdb, start_link, [Realm] },
            permanent, 10000, worker, [fbi_sdb, fbi_sdb_cache]},
        { fbi_utils:atom_for_realm(fbi_server, Realm), { fbi_server, start_link, [Realm] },
            permanent, 10000, worker, [fbi_server, fbi_sdb]}
    ].

reconfigure() ->
    {ok, {_, ChildSpecs}} = init([]),
    superman:reconfigure_supervisor(?MODULE, ChildSpecs).

start_folsom_timer_server(Realm) -> 
    folsom_timer_server:start_link(?FBI_STATS_INTERVAL, fbi_sdb_stats, push_stats, [Realm]).
