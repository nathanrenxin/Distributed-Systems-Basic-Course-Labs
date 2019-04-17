-module(test).
-export([run/2, start/0, stop/0, calc/0]).

run(Sleep, Jitter) ->
%    case whereis(test) of
%        undefined -> ok;
%        _ -> stop()
%    end,
%    start(),
    Log = logger:start([john, paul, ringo, george]),
    A = worker:start(john, Log, 13, Sleep, Jitter),
    B = worker:start(paul, Log, 23, Sleep, Jitter),
    C = worker:start(ringo, Log, 36, Sleep, Jitter),
    D = worker:start(george, Log, 49, Sleep, Jitter),
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    timer:sleep(5000),
%    R=calc(),
    logger:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D). 
%    R.
    
start() ->
    register(test, spawn(fun() ->loop([]) end)).

stop() ->
    test! stop,
    unregister(test).
    
loop(Logs)->
    receive
        stop -> ok;
        {calc, From} -> From ! calc_disorder(Logs);
        Log -> loop(Logs++[Log])
    end.
    
calc() -> 
    test ! {calc, self()},
    receive
        N -> N
    end.
 
 
calc_disorder(Logs) -> calc_disorder(Logs, 0, 0).   
calc_disorder([], 0, 0) -> ok;
%calc_disorder([], N1, N2) -> io:format("test: rate of disorder ~p~n", [N1/N2]);
calc_disorder([], N1, N2) -> N1/N2;
calc_disorder([Log|T], N1, N2) -> 
    case Log of
        {received, Msg} -> 
            calc_disorder(lists:keydelete(Msg,2,T), N1+1, N2+1);
        {sending, Msg} ->
            calc_disorder(lists:keydelete(Msg,2,T), N1, N2+1)
    end.
    