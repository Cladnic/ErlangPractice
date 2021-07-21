-module(save_file_factorial).
-export([factorial/2, factorialRecorder/3, messageRec/0]).

messageRec() ->
    receive
        {factorial, Int} ->
            io:format("Factorial for ~p is ~p ~n",[Int, factorial(Int, 1)]),
            messageRec();
        {factorialRecorder, Int} ->
            {ok, IoDevice} = file:open("C:\\Users\\Johan\\Desktop\\Erlang\\ConC.dat",write),
            factorialRecorder(Int,1,IoDevice),
            file:close(IoDevice),
            messageRec;
        Other ->
            io:format("Invalid Match for ~p~n",[Other]),
            messageRec()
    end.

factorial(Int,Acc) when Int > 0 ->
    factorial(Int-1, Acc * Int);
factorial(0,Acc) ->
    Acc.

factorialRecorder(Int,Acc,IoDevice) when Int > 0 ->
    io:format(IoDevice, "Current Factorial Log: ~p~n",[Acc]),
    factorialRecorder(Int-1,Acc * Int, IoDevice);
factorialRecorder(0,Acc,IoDevice) ->
    io:format(IoDevice, "Factorial Results: ~p~n",[Acc]).
