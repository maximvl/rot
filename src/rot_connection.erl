%%%-------------------------------------------------------------------
%%% @author maxvel <>
%%% @copyright (C) 2014, maxvel
%%% @doc
%%%
%%% @end
%%% Created :  8 Jul 2014 by maxvel <>
%%%-------------------------------------------------------------------
-module(rot_connection).

-behaviour(gen_server).

%% API
-export([start_client/1, start_server/1,
         start_client_link/1, start_server_link/1,
         stop_connection/1, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {kind :: client | server,
                pid :: pid(),
                name :: any(),
                opts :: [any()]}).

%%%===================================================================
%%% API
%%%===================================================================

start_server_link(Opts) ->
  gen_server:start_link(?MODULE, [server, Opts], []).

start_client_link(Opts) ->
  gen_server:start_link(?MODULE, [client, Opts], []).

start_server(Opts) ->
  gen_server:start(?MODULE, [server, Opts], []).

start_client(Opts) ->
  gen_server:start(?MODULE, [client, Opts], []).

stop_connection(Name) ->
  case gproc:lookup_local_name({rot_connection, Name}) of
    undefined -> ok;
    Pid when is_pid(Pid) -> gen_server:cast(Pid, stop)
  end.

send(Name, Data) ->
  case get_worker(Name) of
    Pid when is_pid(Pid) ->
      gen_server:cast(Pid, {send, Data});
    _ -> throw({not_connected, Name})
  end.

get_worker(Node) ->
  case gproc:lookup_pids({p, l, {rot_worker, Node}}) of
    [] -> undefined;
    Procs ->
      lists:nth(random:uniform(length(Procs)), Procs)
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([server, Opts]) ->
  Name = proplists:get_value(name, Opts, node()),
  gproc:add_local_name({rot_connection, Name}),
  Ip = proplists:get_value(ip, Opts, {0, 0, 0, 0}),
  Port = proplists:get_value(port, Opts, 2222),
  Proto = proplists:get_value(transport, Opts, tcp),
  Acceptors = proplists:get_value(acceptors, Opts, 10),
  Jail = proplists:get_value(jail, Opts, undefined),
  Jails = proplists:get_value(jails, Opts, []),
  gproc:add_local_property({rot_server, Name}, [{host, Ip}, {port, Port},
                                                {proto, Proto}, {acceptors, Acceptors},
                                                {jail, Jail}, {jails, Jails}]),

  {ok, Pid} = ranch:start_listener(Name, Acceptors, rot_util:transport(Proto),
                                   [{port, Port}, {ip, Ip}], rot_server,
                                   [{name, Name}, {jail, Jail}, {jails, Jails}]),
  erlang:link(Pid),
  {ok, #state{pid=Pid, kind=server, name=Name,
              opts=[{name, Name}, {host, Ip},
                    {port, Port}, {proto, Proto},
                    {acceptors, Acceptors}, {jail, Jail},
                    {jails, Jails}]}};

init([client, Opts]) ->
  Name = proplists:get_value(name, Opts, node()),
  gproc:add_local_name({rot_connection, Name}),
  Host = proplists:get_value(host, Opts),
  Size = proplists:get_value(size, Opts),
  Proto = proplists:get_value(transport, Opts, tcp),
  Jail = proplists:get_value(jail, Opts),
  {ok, Pid} = rot_pool_sup:start_link(Host, Opts, Size),
  {ok, #state{pid=Pid, kind=client,
              opts=[{name, Name}, {proto, Proto},
                    {workers, Size}, {jail, Jail}]}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({connected, RemoteName, RemoteHost, RemotePort}, #state{opts=Opts}=State) ->
  %% client may be connected to different server on this node
  %% need to check this server only
  case gproc:select(props,
                    [{{{p, l, {rot_connection, RemoteName}}, self(), '_'},
                      [], [true]}]) of
    [] -> gproc:add_local_property({rot_connection, RemoteName},
                                  [{remote_host, RemoteHost},
                                   {remote_port, RemotePort} | Opts]);
    _ -> ok
  end,
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{kind=client, pid=Pid}) ->
  erlang:exit(Pid, shutdown);
terminate(_Reason, #state{kind=server, name=Name}) ->
  ranch:stop_listener(Name).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
