# Write-to-file-factorial
Following code is shell input for running the snippet:

```
Eshell V12.0  (abort with ^G)
1> c(save_file_factorial).       
{ok,save_file_factorial}
2> Pid = spawn(fun save_file_factorial:messageRec/0). 
<0.85.0>
3> Pid ! {factorialRecorder,10}.
{factorialRecorder,10}
```
