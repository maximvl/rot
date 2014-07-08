-module(rot_util).

-export([rpc_call/3, rpc_cast/2, protocol/1, transport/1]).
-export([handle_data/3, pack/2, pack/3]).

rpc_call(Node, MFA, Timeout) ->
  Ref = make_ref(),
  gproc:add_local_name({rot_call, Ref}),
  rot_connection:send(Node, rot_util:pack(call, Ref, MFA)),
  receive
    {'$rot_reply', Ref, Reply} ->
      gproc:unreg({n, l, {rot_call, Ref}}),
      case Reply of
        {'$rot_error', Type, Error, Trace} ->
          erlang:raise(Type, Error, Trace);
        _ ->
          Reply
      end
  after Timeout ->
      gproc:unreg({n, l, {rot_call, Ref}}),
      erlang:exit({timeout, {'rot:call', Node, MFA, Timeout}})
  end.

rpc_cast(Node, MFA) ->
  rot_connection:send(Node, rot_util:pack(cast, MFA)).

handle_data(Node, Data, Jail) ->
  handle_rpc(Node, binary_to_term(Data), Jail).

handle_rpc(_, {'$rot_cast', M, F, A}, undefined) ->
  apply(M, F, A);
handle_rpc(Node, {'$rot_call', Id, M, F, A}, undefined) ->
  handle_call(Node, Id, M, F, A);
handle_rpc(Node, {'$rot_call', Id, _, F, A}, Jail) ->
  handle_call(Node, Id, Jail, F, A);
handle_rpc(_, {'$rot_reply', Id, Data}, _) ->
  case gproc:lookup_local_name({rot_call, Id}) of
    Pid when is_pid(Pid) ->
      Pid ! {'$rot_reply', Id, Data};
    _ -> ok
  end.

handle_call(Node, Id, M, F, A) ->
  Reply = try erlang:apply(M, F, A)
          catch T:E ->
              {'$rot_error', T, E, erlang:get_stacktrace()}
          end,
  rot_connection:send(Node, pack(reply, Id, Reply)).

pack(call, Id, {M, F, A}) ->
  erlang:term_to_binary({'$rot_call', Id, M, F, A});
pack(call, Id, {F, A}) ->
  erlang:term_to_binary({'$rot_call', Id, undefined, F, A});
pack(reply, Id, Reply) ->
  erlang:term_to_binary({'$rot_reply', Id, Reply}).

pack(cast, {M, F, A}) ->
  erlang:term_to_binary({'$rot_cast', M, F, A});
pack(cast, {F, A}) ->
  erlang:term_to_binary({'$rot_cast', undefined, F, A}).

protocol(ranch_tcp) -> tcp;
protocol(ranch_ssl) -> ssl.

transport(tcp) -> ranch_tcp;
transport(ssl) -> ranch_ssl.
