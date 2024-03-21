-module(etskv_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([start_spec/0,
         sup_flags/0,
         child_specs/0]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    start_spec().

start_spec() ->
    {ok, {sup_flags(), child_specs()}}.

sup_flags() ->
    #{strategy => one_for_one,
      intensity => 0,
      period => 1}.

child_specs() ->
    [#{id => etskv_svr,
       start => {etskv_svr, start_link, []},
       restart => permanent,
       shutdown => 2000,
       type => worker,
       modules => [etskv_svr]}].
