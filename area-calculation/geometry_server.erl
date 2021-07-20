-module(geometry_server).
-export([start/0, loop/0]).

start() ->
    spawn(geometry_server, loop, []).

loop() ->
    receive
        {Client, {rectangle, Width, Height}} -> Client ! {self(), Width * Height};
        {Client, {square, Side}} -> Client ! {self(), Side * Side};
        {Client, {circle, Radius}} -> Client ! {self(), Radius * Radius * 3.14159}
    end,
    loop().
