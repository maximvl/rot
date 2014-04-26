-module(rotc).

-export([start/0, stop/0]).
-export([start_connection/4]).
-export([get_pid/1, call/4, cast/4, connected/1, connected/0]).

start() ->
  [application:ensure_started(A) || A <- [ranch, gproc]],
  application:start(rotc).

stop() ->
  application:stop(rotc).

connected() ->
  lists:usort(
    gproc:select(
      props,
      [{{{p, l, {rotc_connection, '$1'}}, '_', '_'}, [], ['$1']}])).

connected(Node) ->
  gproc:select_count(
    props,
    [{{{p, l, {rotc_connection, Node}}, '_', '_'}, [], [true]}]).

start_connection(tcp, Host, Port, Name) ->
  rotc_conn_sup:start_connection([ranch_tcp, Host,
                                  Port, [{name, Name}]]);

start_connection(ssl, Host, Port, Name) ->
  rotc_conn_sup:start_connection([ranch_ssl, Host,
                                  Port, [{name, Name}]]).

call(Node, Module, Fun, Args) ->
  call(Node, Module, Fun, Args, 5000).

call(Node, Module, Fun, Args, Timeout) ->
  case get_pid(Node) of
    Pid when is_pid(Pid) ->
      rotc_connection:call(Pid, {Module, Fun, Args}, Timeout);
    _ ->
      {error, no_pid}
  end.

cast(Node, Module, Fun, Args) ->
  case get_pid(Node) of
    Pid when is_pid(Pid) ->
      rotc_connection:cast(Pid, {Module, Fun, Args});
    _ ->
      {error, no_pid}
  end.

get_pid(Node) ->
  case gproc:lookup_local_properties({rotc_connection, Node}) of
    [] ->
      undefined;
    Procs ->
      element(1, lists:nth(random:uniform(length(Procs)), Procs))
  end.
