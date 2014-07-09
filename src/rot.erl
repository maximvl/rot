-module(rot).

-export([start/0, stop/0]).
-export([call/2, call/3, cast/2,
         connected/1, disconnect/1,
         connections/0, servers/0]).
-export([start_server/1, start_server_link/1,
         connect/2, connect/3,
         connect_link/3, connect_child_spec/3]).

start() ->
  [application:ensure_started(A) ||
    A <- [crypto, asn1, public_key, ssl, ranch, gproc]],
  application:start(rot).

stop() ->
  application:stop(rot).

%% remote connected
connections() ->
  gproc:select(
    props,
    [{{{p, l, {rot_connection, '$1'}}, '$2', '$3'}, [], [{{'$1', '$2', '$3'}}]}]).

connected(Name) ->
  gproc:lookup_local_properties({rot_connection, Name}).

%% local connections
servers() ->
  gproc:select(
    props,
    [{{{p, l, {rot_server, '$1'}}, '$2', '$3'}, [], [{{'$1', '$2', '$3'}}]}]).

call(Node, MFA) ->
  call(Node, MFA, 5000).

call(Node, MFA, Timeout) when tuple_size(MFA) == 3 orelse tuple_size(MFA) == 2 ->
  rot_util:rpc_call(Node, MFA, Timeout).

cast(Node, MFA) when tuple_size(MFA) == 3 orelse tuple_size(MFA) == 2 ->
  rot_util:rpc_cast(Node, MFA).

start_server(Opts) ->
  rot_sup:start_server(Opts).

start_server_link(Opts) ->
  rot_connection:start_server_link(Opts).

connect(Host, Opts) ->
  connect(Host, Opts, 4).

connect(Host, Opts, Size) ->
  rot_sup:start_client([{ip, Host}, {size, Size} | Opts]).

connect_link(Host, Opts, Size) ->
  rot_connection:start_client_link([{ip, Host}, {size, Size} | Opts]).

connect_child_spec(Host, Opts, Size) ->
  Port = proplists:get_value(port, Opts, 2222),
  Id = {Host, Port},
  {Id, {rot_pool_sup, start_link, [Id, Host, Opts, Size]},
   permanent, 5000, supervisor, [rot_pool_sup]}.

%% disconnect: stops client pool or server
disconnect(Name) ->
  rot_connection:stop_connection(Name).
