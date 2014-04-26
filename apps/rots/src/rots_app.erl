-module(rots_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, _} = ranch:start_listener(rots, 100, ranch_tcp,
                                 [{port, 2222},
                                  {active, true},
                                  {packet, 4}],
                                 rots_protocol, [{name, srv1}]),
  rots_sup:start_link().

stop(_State) ->
  ranch:stop_listener(rots).
