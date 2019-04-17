-module(gms3).
-export([start/1, start/2]).

-define(timeout, 2000).
-define(arghh, 20).

start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.
    
init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 0, [], [Master]).
    
start(Id, Grp) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Rnd, Self) end)}.
    
init(Id, Grp, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader|Slaves], Group} = Last ->
            erlang:monitor(process, Leader),
            Master ! {view, Group},
            slave(Id, Master, Leader, N, Last, Slaves, Group)
        after ?timeout ->
            Master ! {error, "no reply from leader"}
    end.
    
leader(Id, Master, N, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2);
        stop ->
            ok
    end.
    
slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, I, _} when I =< N ->
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {view, I, _, _} when I =< N ->
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, I, Msg} = Last1 when I > N ->
            Master ! Msg,
            slave(Id, Master, Leader, I, Last1, Slaves, Group);
        {view, I, [Leader|Slaves2], Group2} = Last1 when I > N ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, I, Last1, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last,Slaves, Group);
        %view from new leader comes before down from old leader
        {view, I, [NewLeader|Slaves2], Group2} = Last1 ->
            erlang:monitor(process, NewLeader),
            Master ! {view, Group2},
            slave(Id, Master, NewLeader, I, Last1, Slaves2, Group2);
        {'DOWN', _Ref, process, _OldLeader, _Reason} ->
            slave(Id, Master, Leader, N, Last, Slaves, Group) ;            
        stop ->
            ok
    end.
 
bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).
    
crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.
    
election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Id, Last, Rest),
            bcast(Id, {view, N+1, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, N+2, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.