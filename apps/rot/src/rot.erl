-module(rot).

-export([start/0, stop/0]).
-export([call/4, call/5, cast/4, get_connection/1,
         connected/0, connected/1]).
-export([start_server/1, stop_server/1,
         connect/5, connect_link/5, connect_child_spec/5]).

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
  {ok, _} = ranch:start_listener(Name, Acceptors, transport(Proto),
                                 [{port, Port},
                                  {ip, Ip}],
                                 rot_worker, [{name, Name}]).

stop_server(Name) ->
  ranch:stop_listener(Name).

connect(Proto, Host, Port, Opts, Size) ->
  poolboy:start(poolboy_opts(Size),
                [client, transport(Proto), Host, Port, Opts]).

connect_link(Proto, Host, Port, Opts, Size) ->
  poolboy:start_link(poolboy_opts(Size),
                     [client, transport(Proto), Host, Port, Opts]).

connect_child_spec(Proto, Host, Port, Opts, Size) ->
  Trans = transport(Proto),
  Id = {Trans, Host, Port},
  poolboy:child_spec(Id, poolboy_opts(Size),
                     [client, Trans, Host, Port, Opts]).

%% TODO disconnect: stop client poolboy, stop workers
%% disconnect(Node) ->
%%   Pids = gproc:select(
%%            props,
%%            [{{{p, l, {rot_connection, Node}}, '$1', '_'}, [], ['$1']}]),
%%   cast(Node, rot, stop, []),
%%   [gen_server:cast(stop, P) || P <- Pids].


%% Utils

poolboy_opts(Size) ->
  [{worker_module, rot_worker},
   {size, Size},
   {max_overflow, 0}].

transport(tcp) -> ranch_tcp;
transport(ssl) -> ranch_ssl.
