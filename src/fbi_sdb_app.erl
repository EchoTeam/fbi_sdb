-module(fbi_sdb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    load_config(),
    fbi_sdb_sup:start_link().

stop(_State) ->
    ok.

load_config() ->
    {ok, InitialRealmsSpec} = application:get_env(fbi_sdb, realms),
    RealmsSpec = extend_realms_spec(InitialRealmsSpec),

    Realms = [R || {R, _} <- RealmsSpec],
    
    Get = fun(Realm, Key) ->
        RS = proplists:get_value(Realm, RealmsSpec, []),
        proplists:get_value(Key, RS)
    end,
    GetNode = fun(Realm, Key) ->
        case Get(Realm, Key) of 
            local ->
                node();
            Value ->
                Value
        end
    end,
    W = fun(L) ->
        lists:zip(L, lists:duplicate(length(L) - 1, ";") ++ ["."])
    end,
    IO = fun(What) ->
        io_lib:format("~p", [What])
    end,
    Mod = fbi_scfg,
    ModSpec = [
            ["-module(", atom_to_list(Mod), ")."],
            ["-export([server_node/1, server_proc/1, sdb_proc/1, sdb_tab/1, sdb_lru_size/1, ",
             "sdb_conf_filename/1, raw_sdb_params/1, server_hostport/1, enumerate/0])."],
            ["enumerate() -> ", IO(Realms), "."],
            [["server_proc(", IO(R), ") -> ", IO(Get(R, server_proc)), D] || {R, D} <- W(Realms)],
            [["server_node(", IO(R), ") -> ", IO(GetNode(R, server_node)), D] || {R, D} <- W(Realms)],
            [["sdb_proc(", IO(R), ") -> ", IO(Get(R, sdb_proc)), D] || {R, D} <- W(Realms)],
            [["sdb_tab(", IO(R), ") -> ", IO(Get(R, sdb_tab)), D] || {R, D} <- W(Realms)],
            [["sdb_lru_size(", IO(R), ") -> ", IO(Get(R, sdb_lru_size)), D] || {R, D} <- W(Realms)],
            [["sdb_conf_filename(", IO(R), ") -> ", IO(Get(R, sdb_conf_filename)), D] || {R, D} <- W(Realms)],
            [["raw_sdb_params(", IO(R), ") -> ", IO(Get(R, raw_sdb_params)), D] || {R, D} <- W(Realms)],
            [["server_hostport(", IO(R), ") -> {", IO(Get(R, server_host)), ",", IO(Get(R, server_port)), "}", D] || {R, D} <- W(Realms)]
    ],
    mod_gen:go(ModSpec).

extend_realms_spec(RealmsSpec) ->
    %% These options will be added to a realm spec (for each realms in the spec)
    %% These options will look in the spec as {option_name, prefix_<realm_short_name>}
    OptionsForExtend = [
        {server_proc, fbi_server_},
        {sdb_proc, fbi_sdb_},
        {sdb_tab, fbi_sdb_flagmap_}
    ],
    lists:map(fun(R) -> extend_realm_spec(R, OptionsForExtend) end, RealmsSpec).

extend_realm_spec({R, Spec}, OptionsForExtend) ->
    ShortName = proplists:get_value(realm_short_name, Spec),
    ConcatAtoms = fun(A1, A2) ->
        list_to_atom(atom_to_list(A1) ++ atom_to_list(A2))
    end,
    GetOption = fun({OptionName, Prefix}) ->
                    {OptionName, ConcatAtoms(Prefix, ShortName)}
    end,
    AdditionalOptions = lists:map(GetOption, OptionsForExtend),
    {R, Spec ++ AdditionalOptions}.
    
