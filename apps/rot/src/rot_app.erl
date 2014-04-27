-module(rot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  case application:get_env(rot, server) of
    {ok, Props} ->
      rot:start_server(Props);
    _ ->
      ok
  end,
  rot_sup:start_link().

stop(_State) ->
  Name = proplists:get_value(name, application:get_env(rot, server, [])),
  rot:stop_server(Name),
  ok.
