-module(node1).
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
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
        after ?Timeout ->
            io:format("Time out: no response~n",[])
    end.

node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor)
    end.


stabilize({_Skey, Spid}) -> Spid ! {request, self()}.
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->
            Successor;
        {Skey, _} ->
             Spid ! {notify, {Id, self()}},
            Successor;           
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    stabilize(Pred, Id, {Xkey, Xpid});
                false ->
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.
    
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).
    
request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.
    
notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            {Nkey, Npid};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    {Nkey, Npid};
                false ->
                    Predecessor
            end
    end.
    
create_probe(Id, {_Skey, Spid}) ->
    Time = erlang:system_time(microsecond),
    Spid ! {probe, Id, [], Time}.
    
forward_probe(Ref, T, Nodes, Id, {_Skey, Spid}) -> 
    Spid ! {probe, Ref, Nodes ++ [Id], T}.
    
remove_probe(T, Nodes)->
    Time = erlang:system_time(microsecond),
    io:format("Probe time used: ~w~n",[Time-T]),
    print_nodes(Nodes).

print_nodes([]) -> ok;
print_nodes([Node|T]) ->
    io:format("Probe node involved: ~w~n",[Node]),
    print_nodes(T).
    