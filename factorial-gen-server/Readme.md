# factorial_gen_server

This is a simple gen_server with client-server interaction and a factorial logic part. The following code is an example of the program running:

```text
Eshell V12.0  (abort with ^G)
1> c(factorial_client). 
{ok,factorial_client}
2> c(factorial_server). 
{ok,factorial_server}
3> c(factorial_logic).  
{ok,factorial_logic}
4> factorial_client:start().       
{local,factorial_server} (<0.95.0>) starting.... 
{ok,<0.95.0>}
5> factorial_client:factorial(34).
295232799039604140847618609643520000000
```

## Client code

```erlang
-module(factorial_client).
-export([start/0, stop/0, factorial/1, factorialRecorder/2]).

start() ->
    factorial_server:start_link().

stop() ->
    factorial_server:stop().

factorial(Val) ->
    factorial_server:factorial(Val).

factorialRecorder(Val, IoDevice) ->
    factorial_server:factorial(Val, IoDevice).

```

## Server code

```erlang
-module(factorial_server).
-behaviour(gen_server).

-export([start_link/0, factorial/1, stop/0, factorial/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%=============Client Call
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast({global, ?MODULE, stop}).

factorial(Val) ->
    gen_server:call({global, ?MODULE},{factorial,Val}).

factorial(Val, IoDevice) ->
    gen_server:call({global, ?MODULE}, {factorial,Val,IoDevice}).

%=============Call back functions
init([]) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) starting.... ~n",[{local, ?MODULE}, self()]),
    {ok, []}.

handle_call({factorial, Val}, _From, State) ->
    {reply, factorial_logic:factorial(Val,1), State};

handle_call({factorial, Val, IoDevice}, _From, State) ->
    {reply, factorial_logic:factorial(Val,1, IoDevice), State};

handle_call(_Request, _From, State) ->
    {reply, i_don_t_know, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, Info, State}.

terminate(_reason, _State) ->
    io:format("terminating ~p~n",[{local, ?MODULE}]),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

```

## Logic code

```erlang
-module(factorial_logic).
-export([factorial_handler/0, factorial/2, factorial/3]).

factorial(Int, Acc) when Int > 0 ->
    factorial(Int-1,Acc * Int);
factorial(0,Acc) ->
    Acc.

factorial(Int,Acc, IoDevice) when Int > 0 ->
    io:format(IoDevice, "CurrentFactorial Log: ~p~n",[Acc]),
    factorial(Int-1,Acc * Int,IoDevice);
factorial(0,Acc,IoDevice) ->
    io:format(IoDevice, "Factorial Results: ~p~n", [Acc]).

factorial_handler() ->
    receive
        {factorial, Int} ->
            io:format("Factorial for ~p is ~p~n",[Int, factorial(Int, 1)]),
            factorial_handler();
        {factorialRecorder, Int, File} ->
            {ok, IoDevice} = file:open(File, write),
            factorial(Int, 1, IoDevice),
            io:format("Factorial Recorder Done. ~n",[]),
            file:close(IoDevice),
            factorial_handler();
        Other ->
            io:format("Invalid Match for ~p~n",[Other]),
            factorial_handler()
    end.
```
