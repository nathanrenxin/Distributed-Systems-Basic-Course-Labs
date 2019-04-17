-module(worker).
-export([start/5, stop/1, peers/2,addpeers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).
    
stop(Worker) ->
    Worker ! stop.
    
init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            Time = vect:zero(),
            loop(Name, Log, Peers, Sleep, Jitter, Time);
        stop ->
           ok
    end.
    
peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.
    
addpeers(Wrk, Peers) ->
    Wrk ! {addpeers, Peers}.
    
loop(Name, Log, Peers, Sleep, Jitter, Time)->
    Wait = random:uniform(Sleep),
    receive
        {addpeers, NewPeers} -> loop(Name, Log, Peers++NewPeers, Sleep, Jitter, Time);
        {msg, TimeR, Msg} ->
            NewTime =  vect:inc(Name,vect:merge(TimeR, Time)),
            Log ! {log, Name, NewTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, NewTime);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
        after Wait ->
            Selected = select(Peers),
            Message = {hello, random:uniform(100)},
            NewTime = vect:inc(Name,Time),
            Selected ! {msg, NewTime, Message},
            jitter(Jitter),
            Log ! {log, Name, NewTime, {sending, Message}},
            loop(Name, Log, Peers, Sleep, Jitter, NewTime)
    end.
    
select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).
    
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).