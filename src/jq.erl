-module(jq).

-export([
           process_json/2
         , set_filter_program_lru_cache_max_size/1
         , get_filter_program_lru_cache_max_size/0
         , implementation_module/0
         , set_implementation_module/1
        ]).
-on_load(init/0).

process_json(FilterProgram, JSONText) ->
    Mod = implementation_module(),
    Mod:process_json(FilterProgram, JSONText).

implementation_module() ->
    persistent_term:get(jq_implementation_module).

set_implementation_module(Module) when Module =:= jq_port; Module =:= jq_nif ->
    persistent_term:put(jq_implementation_module, Module).

set_filter_program_lru_cache_max_size(NewSize) ->
    Mod = implementation_module(),
    Mod:set_filter_program_lru_cache_max_size(NewSize).

get_filter_program_lru_cache_max_size() ->
    Mod = implementation_module(),
    Mod:get_filter_program_lru_cache_max_size().

init() ->
    % %% Load the jq application here since it needs to be loaded so
    % %% we can read its properties
    application:load(jq),
    JQImplementation =
        application:get_env(jq, jq_implementation_module, jq_port),
    set_implementation_module(JQImplementation).
