%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2014 by  <>
%%%-------------------------------------------------------------------
-module(rot_server).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket :: ranch:socket(),
                transport :: module(),
                proto = tcp :: tcp | ssl,
                default_jail :: module() | undefined,
                jails :: [{any(), module()}],
                jail  :: module() | undefined,
                local_name :: any(),
                remote_name :: any()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Ref, Socket, Trans, Opts) ->
  gen_server:start_link(?MODULE, [Ref, Socket, Trans, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Ref, Socket, Trans, Opts]) ->
  JailMod = proplists:get_value(jail, Opts),
  Jails = proplists:get_value(jails, Opts),
  LocalName = proplists:get_value(name, Opts),
  true = LocalName /= undefined,
  Proto = rot_util:protocol(Trans),
  self() ! {init, Ref},
  {ok, #state{socket=Socket, transport=Trans,
              default_jail=JailMod, jails=Jails,
              local_name=LocalName, proto=Proto}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({send, Data}, #state{transport=Trans, socket=Socket}=State) ->
  Trans:send(Socket, Data),
  {noreply, State};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({Proto, Socket, Data}, #state{proto=Proto,
                                          socket=Socket,
                                          transport=Trans,
                                          remote_name=undefined,
                                          default_jail=DefJail,
                                          jails=Jails,
                                          local_name=LName}=State) ->
  case binary_to_term(Data) of
    {reg, RName} ->
      {ok, {RHost, RPort}} = Trans:peername(Socket),
      gproc:add_local_property({rot_worker, RName}, ok),
      gproc:send({n, l, {rot_connection, LName}},
                 {connected, RName, RHost, RPort}),
      Trans:send(Socket, term_to_binary({ok, LName})),
      Jail = proplists:get_value(RName, Jails, DefJail),
      {noreply, State#state{remote_name=RName, jail=Jail}};
    _ ->
      {stop, badreg, State}
  end;

handle_info({Proto, Socket, Data}, #state{socket=Socket,
                                          proto=Proto,
                                          remote_name=Name,
                                          jail=Jail}=State) ->
  proc_lib:spawn(fun() -> rot_util:handle_data(Name, Data, Jail) end),
  {noreply, State};

%% server init
handle_info({init, Ref}, #state{socket=S, transport=T}=State) ->
  ok = ranch:accept_ack(Ref),
  T:setopts(S, [{active, true}, {packet, 4}]),
  {noreply, State};

handle_info({tcp_closed, Socket}, #state{socket=Socket, proto=tcp}=State) ->
  {stop, normal, State};

handle_info({ssl_closed, Socket}, #state{socket=Socket, proto=ssl}=State) ->
  {stop, normal, State};

handle_info({tcp_error, Socket, Reason},
            #state{socket=Socket, proto=tcp}=State) ->
  {stop, Reason, State};

handle_info({ssl_error, Socket, Reason},
            #state{socket=Socket, proto=ssl}=State) ->
  {stop, Reason, State};

handle_info(Info, State) ->
  {stop, {unexpected, Info}, State}.

terminate(_Reason, #state{socket=undefined}) ->
  ok;

terminate(_Reason, #state{transport=Trans, socket=Socket, remote_name=RName, local_name=LName}) ->
  gproc:send({n, l, {rot_connection, LName}}, {disconnected, RName}),
  Trans:close(Socket).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
