
%%%-------------------------------------------------------------------
%% @doc 
%% @end
%%%-------------------------------------------------------------------

-module(jq_port_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [child_spec(Id) || Id <- lists:seq(0, jq_port:nr_of_jq_port_servers() - 1)],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

child_spec(Id) ->
    #{id => Id,
      start => {jq_port, start_link, [Id]}}.

