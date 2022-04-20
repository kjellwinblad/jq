
%%%-------------------------------------------------------------------
%% @doc 
%% @end
%%%-------------------------------------------------------------------

-module(jq_port_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Res = jq_port_sup:start_link(),
    %% Configure the jq port servers once they are up and running
    CacheMaxSize =
        application:get_env(jq, jq_filter_program_lru_cache_max_size, 500),
    jq:set_filter_program_lru_cache_max_size(CacheMaxSize),
    Res.

stop(_State) ->
    ok.

%% internal functions
