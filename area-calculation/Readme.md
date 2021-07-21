# area-calculation

Following code is an example of shell input to run the program:

```
Eshell V12.0  (abort with ^G)
1> c(geometry_server).
{ok,geometry_server}
2> GeometryServer = geometry_server:start(). 
<0.85.0>
3> c(geometry_client). 
{ok,geometry_client}
4> geometry_client:area(GeometryServer, {rectangle, 20, 5}).
100
5> geometry_client:area(GeometryServer, {circle, 6}).        
113.09724
```
