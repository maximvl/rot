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
                jail :: module() | undefined,
                options :: list(),
                host :: any(),
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
  LocalName = proplists:get_value(name, Opts),
  true = LocalName /= undefined,
  Proto = rot_util:protocol(Trans),
  self() ! {init, Ref},
  {ok, #state{socket=Socket, transport=Trans,
              options=Opts, jail=JailMod,
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
                                          local_name=LName}=State) ->
  case binary_to_term(Data) of
    {reg, RName} ->
      Trans:send(Socket, term_to_binary({ok, LName})),
      gproc:add_local_property({rot_connection, RName}, ok),
      {noreply, State#state{remote_name=RName}};
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
  {ok, {Host, _}} = T:peername(S),
  {noreply, State#state{host=Host}};

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

terminate(_Reason, #state{transport=Trans, socket=Socket}) ->
  Trans:close(Socket).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
