-module(geometry_client).
-export([area/2]).

area(Server, Geometry) ->
    Server ! {self(), Geometry},
receive
    {Server, ResultArea} -> ResultArea
end.