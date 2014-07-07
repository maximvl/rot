%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2014 by  <>
%%%-------------------------------------------------------------------
-module(rot_client).

-behaviour(gen_server).

%% API
-export([start_link/2]).

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
%% client
start_link(Host, Opts) ->
  gen_server:start_link(?MODULE, [Host, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Host, Opts]) ->
  Jail = proplists:get_value(jail, Opts),
  LocalName = proplists:get_value(name, Opts, node()),
  Transport = rot_util:transport(proplists:get_value(transport, Opts, tcp)),
  Port = proplists:get_value(port, Opts, 2222),
  case Transport:connect(Host, Port, [{packet, 4}, {active, false}]) of
    {ok, Socket} ->
      Transport:setopts(Socket, [{packet, 4}]),
      Transport:send(Socket, term_to_binary({reg, LocalName})),
      case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
          {ok, RName} = binary_to_term(Data),
          gproc:add_local_property({rot_connection, RName,
                                    [{host, Host}, {port, Port},
                                     {name, LocalName}, {transport, Transport},
                                     {jail, Jail}]}, ok),
          Transport:setopts(Socket, [{active, true}]),
          {ok, #state{jail=Jail, local_name=LocalName,
                      options=Opts, transport=Transport,
                      host=Host, socket=Socket, remote_name=RName}};
        {error, E} ->
          {stop, E}
      end;
    {error, E} ->
      {stop, E}
  end.

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

handle_info({Proto, Socket, Data}, #state{socket=Socket,
                                          proto=Proto,
                                          remote_name=Name,
                                          jail=Jail}=State) ->
  proc_lib:spawn(fun() -> rot_util:handle_data(Name, Data, Jail) end),
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

terminate(_Reason, #state{transport=Trans, socket=Socket}) ->
  Trans:close(Socket).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
