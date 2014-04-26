%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2014 by  <>
%%%-------------------------------------------------------------------
-module(rotc_connection).

-behaviour(gen_server).

%% API
-export([start_link/4, call/3, cast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket :: ranch:socket(),
                transport :: module(),
                host :: any(),
                port :: integer(),
                local_name :: any(),
                remote_name :: any()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Transport, Host, Port, Opts) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE,
                        [Transport, Host, Port, Opts], []).

call(Pid, MFA, Timeout) ->
  Ref = make_ref(),
  gproc:add_local_name({rotc_call, Ref}),
  gen_server:cast(Pid, {send, pack(call, Ref, MFA)}),
  receive
    {'$rot_reply', Ref, Reply} ->
      gproc:unreg({n, l, {rotc_call, Ref}}),
      case Reply of
        {'$rot_error', Type, Error, Trace} ->
          erlang:raise(Type, Error, Trace);
        _ ->
          Reply
      end
  after Timeout ->
      gproc:unreg({n, l, {rotc_call, Ref}}),
      erlang:exit({timeout, {'rotc:call', Pid, MFA, Timeout}})
  end.

cast(Pid, MFA) ->
  gen_server:cast(Pid, {send, pack(cast, MFA)}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Transport, Host, Port, Opts]) ->
  LocalName = proplists:get_value(name, Opts),
  true = LocalName /= false,
  {ok, Socket} = Transport:connect(Host, Port, [{packet, 4},
                                                {active, false}]),
  Transport:send(Socket, term_to_binary({reg, LocalName})),
  case ranch_tcp:recv(Socket, 0, 5000) of
    {ok, Data} ->
      {ok, RName} = binary_to_term(Data),
      gproc:add_local_property({rotc_connection, RName}, ok),
      Transport:setopts(Socket, [{active, true}]),
      {ok, #state{socket=Socket, transport=Transport,
                  host=Host, port=Port,
                  remote_name=RName, local_name=LocalName}};
    E ->
      E
  end.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({send, Data}, #state{transport=Trans, socket=Socket}=State) ->
  Trans:send(Socket, Data),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({Trans, Socket, Data}, #state{socket=Socket,
                                          remote_name=Name}=State) when
    Trans == tcp orelse Trans == ssl ->

  proc_lib:spawn(fun() -> handle_data(Name, Data) end),
  {noreply, State};

handle_info({Msg, Socket}, #state{socket=Socket}=State) when
    Msg == tcp_closed orelse Msg == ssl_closed ->
  {stop, Msg, State};

handle_info({Msg, Socket, Reason}, #state{socket=Socket}=State) when
    Msg == tcp_error orelse Msg == ssl_error ->
  {stop, {Msg, Reason}, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{transport=Trans, socket=Socket}) ->
  Trans:close(Socket).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_data(Name, Data) ->
  handle_command(Name, binary_to_term(Data)).

%% Cast
handle_command(_, {'$rot_cast', M, F, A}) ->
  erlang:apply(M, F, A);

%% Call
handle_command(C, {'$rot_call', R, M, F, A}) ->
  handle_ccall(C, R, M, F, A);

%% Reply
handle_command(_, {'$rot_reply', Ref, Data}) ->
  case gproc:lookup_local_name({rotc_call, Ref}) of
    undefined ->
      ok;
    Pid ->
      Pid ! {'$rot_reply', Ref, Data}
  end.

handle_ccall(C, R, M, F, A) ->
  Reply = try
            erlang:apply(M, F, A)
          catch T:E ->
              {'$rot_error', T, E, erlang:get_stacktrace()}
          end,
  reply(C, term_to_binary({'$rot_reply', R, Reply})).

reply(Client, Data) ->
  case rotc:get_pid(Client) of
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
