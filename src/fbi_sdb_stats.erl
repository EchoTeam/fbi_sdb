-module(fbi_sdb_stats).

-export([
        push_stats/1
        ]).

-define(fbi_sdb_call_timeout, 10000).
-define(fbi_stats_ttl, 90). % in sec

%%% ===================================
%%% API
%%% ===================================

push_stats(Realm) ->
    % FBI stats
    send_stats(collect_fbi_stats(Realm)),
    % FBI metrics 
    Metrics = get_metrics_for_stats(Realm),
    push_metrics(Realm, Metrics).

%%% ===================================
%%% Internal functions
%%% ===================================

push_metrics(Realm, []) -> ok;
push_metrics(Realm, [{M, P}|Rest]) ->
    send_stats(collect_fbi_metrics_stats(Realm, M, P)),
    push_metrics(Realm, Rest).
    
send_stats(Stats) ->
    [stats:notify(K, V, gauge, [{ttl, ?fbi_stats_ttl}]) || {K, V} <- Stats],
    ok.

collect_fbi_stats(Realm) ->
    lager:debug("Started FBI stats collection..."),
    TableStats = fbi_query(Realm, table_stats, undefined),
    NumOrphan = fbi_query(Realm, num_orphan_flags, undefined),
    Stats = case TableStats of
                undefined -> [];
                _ ->
                    [{atom_to_list(Metric), Value} || {Metric, Value} <- TableStats]
            end ++
            case NumOrphan of
                undefined -> [];
                _ -> [{"orphan_flags", NumOrphan}]
            end,
    lager:debug("Done. FBI stats: ~p", [Stats]),
    prepare_stats("fbi.system." ++ atom_to_list(Realm), Stats, "").


collect_fbi_metrics_stats(Realm, Category, Mask) ->
    lager:debug("Started FBI metrics collection..."),
    RawStats = get_fbi_groupped_stats(Realm, Category ++ Mask, []),
%   Example of RawStats:
%   [{"jskit/all", [{all,{activity_metrics,[
%       {miss, [
%           {rate, [0,0,0]},
%           {latency, [0,0,0]}
%       ]},
%       {hit, [
%           {rate, [3303,17031,13905902]},
%           {latency, [0,0,0]}
%       ]}
%   ]}}]}]
    PrepareValues = fun(Name, D) ->
        [RM, R5M, RH] = proplists:get_value(rate, D),
        [LM, L5M, LH] = proplists:get_value(latency, D),
        [{[Name, "rate", "1min"], RM},
         {[Name, "rate", "5min"], R5M},
         {[Name, "rate", "24h"],  RH},
         {[Name, "latency", "1min"], LM},
         {[Name, "latency", "5min"], L5M},
         {[Name, "latency", "24h"],  LH}]
    end,
    Stats = lists:flatten(lists:map(fun({Metric, [{all, {activity_metrics, S}}]}) ->
        Values = PrepareValues("hit",  proplists:get_value(hit, S)) ++
            PrepareValues("miss", proplists:get_value(miss, S)),
        [
            {construct_counter_name(string:tokens(Category, "/")
                ++ string:tokens(Metric, "/") ++ K), V} ||
            {K, V} <- Values,
            V /= 0
        ]
    end, RawStats)),
    lager:debug("Done. Collected ~p FBI metrics", [length(Stats)]),
    prepare_stats("fbi.metrics", Stats, "").

fbi_query(Realm, Query, Default) ->
    try fbi_sdb:sdb_query(Realm, Query, ?fbi_sdb_call_timeout) of
        Result ->
            lager:debug("[fbi_query]. Realm: ~p Query: ~p Result: ~p", [Realm, Query, Result]),
            Result
    catch C:R ->
        lager:warning("[fbi_query] (Realm: ~p) Failed to execute query:~n~p:~p", [Realm, C, R]),
        Default
    end.

prepare_stats(Prefix, Stats, Suffix) ->
    Join =
        fun (Keys) ->
            string:join(lists:filter(fun (V) -> V =/= "" end, Keys), ".")
        end,
    [{Join([Prefix, Key, Suffix]), Value} || {Key, Value} <- Stats].

get_fbi_groupped_stats(Realm, Pattern, Categories) ->
    try fbi_sdb:sdb_query(Realm, {groupped_metric_stats, Pattern, 0, all, Categories, 60}, ?fbi_sdb_call_timeout) of
        List -> List
    catch C:R ->
        lager:warning("Failed to get information: ~n~p:~p", [C, R]),
        []
    end.

construct_counter_name(List) ->
    string:join([stats:safe_string(S) || E <- List, S <- [type_utils:to_list(E)], S =/= ""], ".").

get_metrics_for_stats(Realm) ->
    SpecialConfigs = application:get_env(fbi_sdb, special, []),
    RealmConfigs = proplists:get_value(Realm, SpecialConfigs, []),
    proplists:get_value(metrics, RealmConfigs, []).
