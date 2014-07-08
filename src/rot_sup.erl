-module(rot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_server/1]).
-export([start_client/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, M, Args),
        {Id, {M, start_link, Args}, transient, 5000, worker, [M]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Opts) ->
  case erlang:whereis(?MODULE) of
    undefined -> rot_connection:start_server(Opts);
    Pid -> supervisor:start_child(Pid, [server, Opts])
  end.

start_client(Opts) ->
  case erlang:whereis(?MODULE) of
    unefined -> rot_connection:start_client(Opts);
    Pid -> supervisor:start_child(Pid, [client, Opts])
  end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  S = ?CHILD(rot_connections, rot_connection, []),
  {ok, { {simple_one_for_one, 5, 10}, [S]} }.
