##About

Rot is RPC over TCP client/server application which allowes transparent parallel synchronious and asynchronious requests between nodes.
Each erlang node can have one Rot server and several connections to other nodes, under different names.

Start server on node1:
```
(node1@t530)1> rot:start_server([{name, srv}]).
{ok,<0.230.0>}
```

Connect and call from node2:
```
(node2@t530)1> rot:connect(tcp, localhost, 2222, [{name, cl}], 5).
{ok,<0.153.0>}
(node2@t530)2> rot:connected().
[srv]
(node2@t530)3> rot:call(srv, erlang, self, []).
<12820.366.0>
```

After connect on node1:
```
(node1@t530)2> rot:connected().
[cl]
(node1@t530)3> rot:call(cl, erlang, self, []).
<12756.340.0>
```

####Technologies used:
* https://github.com/extend/ranch - server connections pool
* https://github.com/devinus/poolboy - client connections pool
* https://github.com/uwiger/gproc - connections processes registration
* https://github.com/rustyio/sync, https://github.com/massemanet/eper - non-stop development and debug

##Jails

Both server and client can set ```{jail, Module}``` option which restricts all requests to that module, ignoring module passed in requests:

```
(node1@t530)1> rot:start_server([{name, srv}, {jail, ets}]).
{ok,<0.115.0>}
```

node2:
```
(node2@t530)1> rot:connect(tcp, localhost, 2222, [{name, cl}], 5).
{ok,<0.118.0>}
(node2@t530)2> rot:call(srv, erlang, memory, []).
** exception error: undefined function ets:memory/0
     in function  rot_util:handle_call/5 (rot/src/rot_util.erl, line 45)
     in call from proc_lib:init_p/3 (proc_lib.erl, line 224)
(node2@t530)3> rot:call(srv, erlang, module_info, []).
[{exports,[{match_spec_run,2},
           {repair_continuation,2},
           {fun2ms,1},
           {foldl,3},
           {foldr,3},
           ...
```

##Todo

###Jails:
* different jails for different clients on server
* default jail

###Nodes:
* disconnect (server forces clients?)
* nodes graph walking
* finding shortest paths
* paths caching, recovering broken paths
