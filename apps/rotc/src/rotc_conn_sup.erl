%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 26 Apr 2014 by  <>
%%%-------------------------------------------------------------------
-module(rotc_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_connection/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_connection(Opts) ->
  supervisor:start_child(?MODULE, Opts).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
  AChild = ?CHILD(rotc_connection, worker),
  {ok, { {simple_one_for_one, 5, 10}, [AChild] } }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
