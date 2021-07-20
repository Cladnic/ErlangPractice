# Erlang
Repository for code-snippets created while learning Erlang. Content varies and consists for example of the following simple client-server interaction:

```erlang
-module(changecase_client).
-export([changecase/3]).

changecase(Server, Str, Command) ->
    Server ! {self(), {Str, Command}},
    receive 
        {Server, ResultString} -> ResultString
    end.

```

```erlang
-module(changecase_server).
-export([start/0, loop/0]).

start() ->
    spawn(changecase_server, loop, []).

loop() ->
    receive
        {Client, {Str, uppercase}} -> Client ! {self(), string:to_upper(Str)}; 
        {Client, {Str, lowercase}} -> Client ! {self(), string:to_lower(Str)}
    end,
    loop().

```
