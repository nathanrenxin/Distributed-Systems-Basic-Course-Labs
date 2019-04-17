-module(node3).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 10000).

start(Id) ->
    start(Id, nil).
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).
    
init(Id, Peer) ->
    Predecessor = nil,
    {ok, {Skey, Spid}, Next} = connect(Id, Peer),
    Ref = monitor(Spid),
    schedule_stabilize(),
    node(Id, Predecessor, {Skey, Ref, Spid}, [], Next).

connect(Id, nil) ->
    {ok, {Id, self()}, {Id, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey, Next} ->
            {ok, {Skey, Peer}, Next}
        after ?Timeout ->
            io:format("Time out: no response~n",[])
    end.

node(Id, Predecessor, Successor, Store, Next) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id, Successor},
            node(Id, Predecessor, Successor, Store, Next);
        {notify, New} ->
            {Pred, Keep} = notify(New, Id, Predecessor, Store),     
            node(Id, Pred, Successor, Keep, Next);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {status, Pred, Nxt} ->
            {Succ, Nx} = stabilize(Pred, Id, Successor, Nxt),
            node(Id, Predecessor, Succ, Store, Nx);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next);
        print ->
            print(Predecessor, Id, Successor, Next),
            node(Id, Predecessor, Successor, Store, Next);            
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor,Store, Next);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor,Store, Next);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
            Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Next);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Next);
        {'DOWN', Ref, process, _, _} ->
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Store, Nxt)            
    end.


stabilize({_Skey, _Ref, Spid}) -> Spid ! {request, self()}.
stabilize(Pred, Id, Successor, Next) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Id, _} ->
            {Successor, Next};
        {Skey, _} ->
             Spid ! {notify, {Id, self()}},
            {Successor, Next};        
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    drop(Sref),
                    Xref= monitor(Xpid),
                    stabilize(Pred, Id, {Xkey, Xref, Xpid}, {Skey, Spid});
                false ->
                    Spid ! {notify, {Id, self()}},
                    {Successor, Next}
            end
    end.
    
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).
    
request(Peer, Predecessor, {Skey, _Sref, Spid}) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Spid}};
        {Pkey, _Ref, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
    end.
    
notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            Nref = monitor(Npid),
            {{Nkey, Nref, Npid}, Keep};
        {Pkey, Pref, Ppid} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    drop(Pref),
                    Nref=monitor(Npid),
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Nref, Npid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Nkey, Id, Store),
    Npid ! {handover, Rest},
    Keep.
    
create_probe(Id, {_Skey, _, Spid}) ->
    Time = erlang:system_time(microsecond),
    Spid ! {probe, Id, [], Time}.
    
forward_probe(Ref, T, Nodes, Id, {_Skey, _, Spid}) -> 
    Spid ! {probe, Ref, Nodes ++ [Id], T}.
    
remove_probe(T, Nodes)->
    Time = erlang:system_time(microsecond),
    io:format("Probe time used: ~w~n",[Time-T]),
    print_nodes(Nodes).

print_nodes([]) -> ok;
print_nodes([Node|T]) ->
    io:format("Probe node involved: ~w~n",[Node]),
    print_nodes(T).
  
  
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid!{add, Key, Value, Qref, Client},
            Store
    end.  
    
lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, _, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.    
    
monitor(Pid) ->
    erlang:monitor(process, Pid).
    
drop(nil) ->
    ok;
drop(Ref) ->
    erlang:demonitor(Ref, [flush]).    
    
    
down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
    Nref = monitor(Npid),
    {Predecessor, {Nkey, Nref, Npid}, nil}.
    
    
print(Predecessor, Id, Successor, Next) ->
    io:format("Predecessor: ~w~n",[Predecessor]),
    io:format("Own Id: ~w~n",[Id]),
    io:format("Successor: ~w~n",[Successor]),
    io:format("Next: ~w~n",[Next]).