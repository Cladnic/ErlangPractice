# uppercase-lowercase-conversion
Following code is shell input to run the program:

```
Eshell V12.0  (abort with ^G)
1> c(changecase_server).
{ok,changecase_server}
2> ChangeCaseServer = changecase_server:start().
<0.85.0>
3> c(changecase_client).
{ok,changecase_client}
4> changecase_client:changecase(ChangeCaseServer, "HELLO", lowercase).
"hello"
5> changecase_client:changecase(ChangeCaseServer, "hello", uppercase). 
"HELLO"
```

