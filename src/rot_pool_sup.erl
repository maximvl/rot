-module(rot_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).
-export([start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, M, Args), {Id, {M, start_link, Args}, permanent, 5000, worker, [M]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link(Host, Opts, Size) ->
  {ok, Pid} = supervisor:start_link(?MODULE, [Host, Opts]),
  AllStarted = lists:all(
                 fun({R, _}) -> R == ok end,
                 [rot_pool_sup:start_child(Pid) ||
                   _ <- lists:seq(1, Size)]),
  %% have to check initial connections
  %% because superviser does not restart them
  if AllStarted ->
      {ok, Pid};
     true ->
      erlang:exit(Pid, shutdown),
      {error, connection_failed}
  end.

start_child(Pid) ->
  supervisor:start_child(Pid, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Host, Opts]) ->
  Child = ?CHILD(id, rot_client, [Host, Opts]),
  {ok, { {simple_one_for_one, 5, 10}, [Child]} }.
