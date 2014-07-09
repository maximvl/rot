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
-export([start_client/1, start_server/1, start_link/2,
         start_client_link/1, start_server_link/1,
         stop_connection/1, send/2]).

-export([fill_default_opts/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {kind :: client | server,
                pid :: pid() | reference(),
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

start_link(Kind, Opts) ->
  gen_server:start_link(?MODULE, [Kind, Opts], []).

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
  Defaults = [{name, node()}, {ip, {0, 0, 0, 0}},
              {port, 2222}, {proto, tcp},
              {acceptors, 10}, {jail, undefined}, {jails, []}],
  RealOpts = fill_default_opts(Opts, Defaults),
  Name = get_prop(name, RealOpts),
  gproc:add_local_name({rot_connection, Name}),
  gproc:add_local_property({rot_server, Name}, RealOpts),
  Proto = get_prop(proto, RealOpts),
  Ip = get_prop(ip, RealOpts),
  Port = get_prop(port, RealOpts),
  RanchOpts = case Proto of
                ssl ->
                  CertFile = get_prop(cacertfile, RealOpts),
                  PKFile = get_prop(keyfile, RealOpts),
                  [{port, Port}, {ip, Ip},
                   {cacertfile, CertFile}, {keyfile, PKFile},
                   {verify, verify_peer}];
                tcp -> [{port, Port}, {ip, Ip}]
              end,
  Acceptors = get_prop(acceptors, RealOpts),
  {ok, Pid} = ranch:start_listener(Name, Acceptors, rot_util:transport(Proto),
                                   RanchOpts, rot_server, RealOpts),
  Ref = erlang:monitor(process, Pid),
  {ok, #state{pid=Ref, kind=server, name=Name,
              opts=RealOpts}};

init([client, Opts]) ->
  Defaults = [{name, node()}, {port, 2222},
              {proto, tcp}, {size, 4},
              {jail, undefined}, {jails, []}],
  RealOpts = fill_default_opts(Opts, Defaults),
  Name = get_prop(name, RealOpts),
  gproc:add_local_name({rot_connection, Name}),
  Host = get_prop(ip, RealOpts),
  Size = get_prop(size, RealOpts),
  {ok, Pid} = rot_pool_sup:start_link(Host, RealOpts, Size),
  {ok, #state{pid=Pid, kind=client, opts=RealOpts}}.

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
  %% what to do if different clients connect with same name?
  case gproc:select(props,
                    [{{{p, l, {rot_connection, RemoteName}}, self(), '_'},
                      [], [true]}]) of
    [] -> gproc:add_local_property({rot_connection, RemoteName},
                                  [{remote_host, RemoteHost},
                                   {remote_port, RemotePort} | Opts]);
    _ -> ok
  end,
  {noreply, State};

handle_info({disconnected, RemoteName}, State) ->
  %% throws error when property does not present
  catch gproc:unreg({p, l, {rot_connection, RemoteName}}),
  {noreply, State};

handle_info({'DOWN', MonitorRef, process, _Object, Info}, #state{pid=MonitorRef}=State) ->
  {stop, Info, State};

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

fill_default_opts(Plist, Defaults) ->
  {L1, L2} = lists:foldl(fun({K, V}, {Vals, Acc}) ->
                             case lists:keytake(K, 1, Acc) of
                               false -> V2 = V, Acc2 = Acc;
                               {value, {K, V2}, Acc2} -> ok
                             end,
                             {[{K, V2} | Vals], Acc2}
                         end, {[], Plist}, Defaults),
  L1 ++ L2.

get_prop(K, L) -> get_prop(K, L, undefined).
get_prop(K, L, V) ->
  case lists:keyfind(K, 1, L) of
    false -> V;
    {K, V2} -> V2
  end.
