-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).
    
stop(Logger) ->
    Logger ! stop.
    
init(Nodes) ->
    loop(time:clock(Nodes), []).
    
loop(Clock, Queue) ->
    receive
        {log, From, Time, Msg} ->
            NewClock = time:update(From, Time, Clock),
            NewQueue = log(lists:keysort(1, Queue ++ [{Time, From, Msg}]), NewClock),            
            loop(NewClock, NewQueue);
        stop ->
            ok
    end.

log(Queue, Clock) -> log(Queue, Clock, []).   
 
log([], _Clock, NewQueue) -> NewQueue;
log([{Time, From, Msg}|T], Clock, NewQueue) ->
%    case whereis(test) of
%        undefined -> ok;
%        _ -> test! Msg
%    end,
    case time:safe(Time, Clock) of
        true -> 
            io:format("log: ~w ~w ~p~n", [Time, From, Msg]),
            log(T, Clock, NewQueue);
        false -> 
            log(T, Clock, NewQueue++[{Time, From, Msg}])
    end.
