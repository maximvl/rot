-module(rot_pool_sup).

-behaviour(supervisor).

%% API
-export([start/4]).
-export([start_link/4]).
-export([start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, M, Args), {Id, {M, start_link, Args}, permanent, 5000, worker, [M]}).

%% ===================================================================
%% API functions
%% ===================================================================
start(Name, Host, Opts, Size) ->
  start_link(Name, Host, Opts, Size, true).

start_link(Name, Host, Opts, Size) ->
  start_link(Name, Host, Opts, Size, false).

start_link(Name, Host, Opts, Size, Unlink) ->
  case gproc:lookup_local_name({rot_pool, Name}) of
    undefined ->
      {ok, Pid} = supervisor:start_link(?MODULE, [Name, Host, Opts]),
      AllStarted = lists:all(
                     fun({R, _}) -> R == ok end,
                     [rot_pool_sup:start_child(Pid) ||
                       _ <- lists:seq(1, Size)]),
      if AllStarted ->
          Unlink andalso erlang:unlink(Pid),
          {ok, Pid};
         true ->
          erlang:unlink(Pid),
          erlang:exit(Pid, shutdown),
          {error, connection_failed}
      end;
    P ->
      {already_started, P}
  end.

start_child(Pid) ->
  supervisor:start_child(Pid, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Name, Host, Opts]) ->
  gproc:add_local_name({rot_pool, Name}),
  Child = ?CHILD(id, rot_client, [Host, Opts]),
  {ok, { {simple_one_for_one, 5, 10}, [Child]} }.
