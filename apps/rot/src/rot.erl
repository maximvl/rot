-module(rot).

-export([start/0, stop/0]).
-export([call/4, call/5, cast/4, get_connection/1,
         connected/0, connected/1]).
-export([start_server/1, stop_server/1,
         connect/2, connect/3,
         connect_link/3, connect_child_spec/3]).

start() ->
  [application:ensure_started(A) ||
    A <- [crypto, asn1, public_key, ssl, ranch, gproc]],
  application:start(rot).

stop() ->
  application:stop(rot).

connected() ->
  lists:usort(
    gproc:select(
      props,
      [{{{p, l, {rot_connection, '$1'}}, '_', '_'}, [], ['$1']}])).

connected(Node) ->
  gproc:select_count(
    props,
    [{{{p, l, {rot_connection, Node}}, '_', '_'}, [], [true]}]).

call(Node, Module, Fun, Args) ->
  call(Node, Module, Fun, Args, 5000).

call(Node, Module, Fun, Args, Timeout) ->
  case get_connection(Node) of
    Pid when is_pid(Pid) ->
      rot_util:rpc_call(Pid, {Module, Fun, Args}, Timeout);
    _ ->
      {error, no_connection}
  end.

cast(Node, Module, Fun, Args) ->
  case get_connection(Node) of
    Pid when is_pid(Pid) ->
      rot_util:rpc_cast(Pid, {Module, Fun, Args});
    _ ->
      {error, no_connection}
  end.

get_connection(Node) ->
  case gproc:lookup_local_properties({rot_connection, Node}) of
    [] ->
      undefined;
    Procs ->
      element(1, lists:nth(random:uniform(length(Procs)), Procs))
  end.

start_server(Props) ->
  Ip = proplists:get_value(ip, Props, {0, 0, 0, 0}),
  Port = proplists:get_value(port, Props, 2222),
  Name = proplists:get_value(name, Props, node()),
  Proto = proplists:get_value(transport, Props, tcp),
  Acceptors = proplists:get_value(acceptors, Props, 10),
  Jail = proplists:get_value(jail, Props, undefined),
  {ok, _} = ranch:start_listener(Name, Acceptors, rot_util:transport(Proto),
                                 [{port, Port}, {ip, Ip}],
                                 rot_server, [{name, Name}, {jail, Jail}]).

stop_server(Name) ->
  ranch:stop_listener(Name).

connect(Host, Opts) ->
  connect(Host, Opts, 4).

connect(Host, Opts, Size) ->
  Port = proplists:get_value(port, Opts, 2222),
  rot_pool_sup:start({Host, Port}, Host, Opts, Size).

connect_link(Host, Opts, Size) ->
  Port = proplists:get_value(port, Opts, 2222),
  rot_pool_sup:start_link({Host, Port}, Host, Opts, Size).

connect_child_spec(Host, Opts, Size) ->
  Port = proplists:get_value(port, Opts, 2222),
  Id = {Host, Port},
  {Id, {rot_pool_sup, start_link, [Id, Host, Opts, Size]},
   permanent, 5000, supervisor, [rot_pool_sup]}.

%% TODO disconnect: stop client pool, stop workers
%% disconnect(Node) ->
%%   Pids = gproc:select(
%%            props,
%%            [{{{p, l, {rot_connection, Node}}, '$1', '_'}, [], ['$1']}]),
%%   cast(Node, rot, stop, []),
%%   [gen_server:cast(stop, P) || P <- Pids].
