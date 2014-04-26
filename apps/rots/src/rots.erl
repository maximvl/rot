-module(rots).

-export([start/0, stop/0]).
-export([call/4, call/5, cast/4, get_pid/1,
         connected/0, connected/1]).

start() ->
  [application:ensure_started(A) || A <- [ranch, gproc]],
  application:start(rots).

stop() ->
  application:stop(rots).

connected() ->
  lists:usort(
    gproc:select(
      props,
      [{{{p, l, {rots_connection, '$1'}}, '_', '_'}, [], ['$1']}])).

connected(Node) ->
  gproc:select_count(
    props,
    [{{{p, l, {rots_connection, Node}}, '_', '_'}, [], [true]}]).

call(Node, Module, Fun, Args) ->
  call(Node, Module, Fun, Args, 5000).

call(Node, Module, Fun, Args, Timeout) ->
  case get_pid(Node) of
    Pid when is_pid(Pid) ->
      rots_protocol:call(Pid, {Module, Fun, Args}, Timeout);
    _ ->
      {error, no_pid}
  end.

cast(Node, Module, Fun, Args) ->
  case get_pid(Node) of
    Pid when is_pid(Pid) ->
      rots_protocol:cast(Pid, {Module, Fun, Args});
    _ ->
      {error, no_pid}
  end.

get_pid(Node) ->
  case gproc:lookup_local_properties({rots_connection, Node}) of
    [] ->
      undefined;
    Procs ->
      element(1, lists:nth(random:uniform(length(Procs)), Procs))
  end.
