%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 24 Apr 2014 by  <>
%%%-------------------------------------------------------------------
-module(rots_protocol).

-behaviour(ranch_protocol).
-behaviour(gen_server).

%% API
-export([start_link/4, call/3, cast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket    :: ranch:socket(),
                transport :: module(),
                options   :: list(),
                jail      :: module() | false,
                local_name :: any(),
                remote_name :: any()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Socket, Trans, Opts) ->
  gen_server:start_link(?MODULE, [Ref, Socket, Trans, Opts], []).

call(Pid, MFA, Timeout) ->
  Ref = make_ref(),
  gproc:add_local_name({rots_call, Ref}),
  gen_server:cast(Pid, {send, pack(call, Ref, MFA)}),
  receive
    {'$rot_reply', Ref, Reply} ->
      gproc:unreg({n, l, {rots_call, Ref}}),
      case Reply of
        {'$rot_error', Type, Error, Trace} ->
          erlang:raise(Type, Error, Trace);
        _ ->
          Reply
      end
    after Timeout ->
        gproc:unreg({n, l, {rots_call, Ref}}),
        erlang:exit({timeout, {'rots:call', Pid, MFA, Timeout}})
  end.

cast(Pid, MFA) ->
  gen_server:cast(Pid, {send, pack(cast, MFA)}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Ref, Socket, Trans, Opts]) ->
  JailMod = proplists:get_value(jail, Opts),
  LocalName = proplists:get_value(name, Opts),
  true = LocalName /= false,
  self() ! {init, Ref},
  {ok, #state{socket=Socket, transport=Trans,
              options=Opts, jail=JailMod,
              local_name=LocalName}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({send, Data}, #state{transport=Trans, socket=Socket}=State) ->
  Trans:send(Socket, Data),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({T, Socket, Data}, #state{transport=Trans,
                                      socket=Socket,
                                      remote_name=undefined,
                                      local_name=LName}=State) when
    T == tcp orelse T == ssl ->
  case binary_to_term(Data) of
    {reg, RName} ->
      Trans:send(Socket, term_to_binary({ok, LName})),
      gproc:add_local_property({rots_connection, RName}, ok),
      {noreply, State#state{remote_name=RName}};
    _ ->
      {stop, badreg, State}
  end;

handle_info({Trans, Socket, Data}, #state{socket=Socket,
                                          jail=Jail,
                                          remote_name=Name}=State) when
    Trans == tcp orelse Trans == ssl ->
  spawn(fun() -> handle_data(Name, Data, Jail) end),
  {noreply, State};

handle_info({init, Ref}, #state{socket=S, transport=T}=State) ->
  ok = ranch:accept_ack(Ref),
  T:setopts(S, [{active, true}]),
  {noreply, State};

handle_info({Msg, Socket}, #state{socket=Socket}=State) when
    Msg == tcp_closed orelse Msg == ssl_closed ->
  {stop, Msg, State};

handle_info({Msg, Socket, Reason}, #state{socket=Socket}=State) when
    Msg == tcp_error orelse Msg == ssl_error ->
  {stop, {Msg, Reason}, State};

handle_info(_Info, State) ->
  error_logger:error_msg("info: ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, #state{transport=Trans, socket=Socket}) ->
  Trans:close(Socket).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_data(Client, Data, Jail) ->
  handle_command(Client, binary_to_term(Data), Jail).

%% Cast
%% all code allowed
handle_command(_, {'$rot_cast', M, F, A}, undefined) ->
  apply(M, F, A);
%% Jail
handle_command(_, {'$rot_cast', _, F, A}, M) ->
  apply(M, F, A);
handle_command(_, {'$rot_cast', F, A}, M) ->
  apply(M, F, A);

%% Call
%% allowed
handle_command(C, {'$rot_call', R, M, F, A}, undefined) ->
  handle_ccall(C, R, M, F, A);
%% Jail
handle_command(C, {'$rot_call', R, _, F, A}, M) ->
  handle_ccall(C, R, M, F, A);
handle_command(C, {'$rot_call', R, F, A}, M) ->
  handle_ccall(C, R, M, F, A);

handle_command(_, {'$rot_reply', Ref, Data}, _) ->
  case gproc:lookup_local_name({rots_call, Ref}) of
    undefined ->
      ok;
    Pid ->
      Pid ! {'$rot_reply', Ref, Data}
  end.

handle_ccall(C, R, M, F, A) ->
  Reply = try erlang:apply(M, F, A)
          catch T:E ->
              {'$rot_error', T, E, erlang:get_stacktrace()}
          end,
  reply(C, term_to_binary({'$rot_reply', R, Reply})).

reply(Client, Data) ->
  case rots:get_pid(Client) of
    Pid when is_pid(Pid) ->
      gen_server:cast(Pid, {send, Data});
    _ ->
      ok
  end.

pack(call, Ref, {M, F, A}) ->
  erlang:term_to_binary({'$rot_call', Ref, M, F, A});
pack(call, Ref, {F, A}) ->
  erlang:term_to_binary({'$rot_call', Ref, F, A}).

pack(cast, {M, F, A}) ->
  erlang:term_to_binary({'$rot_cast', M, F, A});
pack(cast, {F, A}) ->
  erlang:term_to_binary({'$rot_cast', F, A}).
