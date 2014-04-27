%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2014 by  <>
%%%-------------------------------------------------------------------
-module(rot_worker).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket :: ranch:socket(),
                transport :: module(),
                proto = tcp :: tcp | ssl,
                jail :: module() | undefined,
                options :: list(),
                local_name :: any(),
                remote_name :: any()}).

%%%===================================================================
%%% API
%%%===================================================================
%% client
start_link([client, Trans, Host, Port, Opts]) ->
  gen_server:start_link(?MODULE, [client, Trans, Host, Port, Opts], []).

%% server
start_link(Ref, Socket, Trans, Opts) ->
  gen_server:start_link(?MODULE, [server, Ref, Socket, Trans, Opts], []).


%% start_server(Ref, Socket, Trans, Opts) ->
%%   gen_server:start_link({local, ?SERVER}, ?MODULE,
%%                         [server, Ref, Socket, Trans, Opts], []).

%% start_client(Transport, Host, Port, Opts) ->
%%   gen_server:start_link({local, ?SERVER}, ?MODULE,
%%                         [client, Transport, Host, Port, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([client, Transport, Host, Port, Opts]) ->
  Jail = proplists:get_value(jail, Opts),
  LocalName = proplists:get_value(name, Opts, node()),
  {ok, Socket} = Transport:connect(Host, Port, [{packet, 4},
                                                {active, false}]),
  Transport:setopts(Socket, [{packet, 4}]),
  Transport:send(Socket, term_to_binary({reg, LocalName})),
  case Transport:recv(Socket, 0, 5000) of
    {ok, Data} ->
      {ok, RName} = binary_to_term(Data),
      gproc:add_local_property({rot_connection, RName}, ok),
      Transport:setopts(Socket, [{active, true}]),
      Proto = rot_util:protocol(Transport),
      {ok, #state{socket=Socket, transport=Transport,
                  options=Opts, jail=Jail, proto=Proto,
                  remote_name=RName, local_name=LocalName}};
    E ->
      E
  end;

init([server, Ref, Socket, Trans, Opts]) ->
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

handle_info({init, Ref}, #state{socket=S, transport=T}=State) ->
  ok = ranch:accept_ack(Ref),
  T:setopts(S, [{active, true}, {packet, 4}]),
  {noreply, State};

%% handle_info({tcp_closed, Socket}, #state{socket=Socket, proto=tcp}=State) ->
%%   {stop, tcp_closed, State};

%% handle_info({ssl_closed, Socket}, #state{socket=Socket, proto=ssl}=State) ->
%%   {stop, ssl_closed, State};

%% handle_info({tcp_error, Socket, Reason},
%%             #state{socket=Socket, proto=tcp}=State) ->
%%   {stop, {tcp_error, Reason}, State};

%% handle_info({ssl_error, Socket, Reason},
%%             #state{socket=Socket, proto=ssl}=State) ->
%%   {stop, {ssl_error, Reason}, State};

handle_info(Info, State) ->
  {stop, {unexpected, Info}, State}.

terminate(_Reason, #state{transport=Trans, socket=Socket}) ->
  Trans:close(Socket).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
