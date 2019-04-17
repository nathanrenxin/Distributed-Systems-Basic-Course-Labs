-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() -> 0.

inc(Name, T) -> T+1.

merge(Ti, Tj) -> max(Ti, Tj).

leq(Ti,Tj) -> Ti=<Tj. 

clock(Nodes) -> [{X, 0} || X<-Nodes].

update(Node, Time, Clock) -> lists:keyreplace(Node, 1, Clock, {Node, Time}).

safe(Time, Clock) -> 
    leq(Time, lists:min([X|| {_Name,X} <- Clock])).
