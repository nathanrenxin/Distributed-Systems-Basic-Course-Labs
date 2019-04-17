-module(intf).
-compile(export_all).

new() -> [].

add(Name, Ref, Pid, Intf) -> Intf ++ [{Name, Ref, Pid}].

remove(Name, Intf) -> lists:keydelete(Name,1,Intf).

lookup(Name, Intf) ->
    case lists:keyfind(Name, 1 , Intf) of
        false -> notfound;
        {Name, _Ref, Pid} -> {ok, Pid}
    end.
    
ref(Name, Intf) -> 
    case lists:keyfind(Name, 1 , Intf) of
        false -> notfound;
        {Name, Ref, _Pid} -> {ok, Ref}
    end.
    
name(Ref, Intf) ->
    case lists:keyfind(Ref, 2 , Intf) of
        false -> notfound;
        {Name, Ref, _Pid} -> {ok, Name}
    end.
    
list(Intf) -> 
    [X|| {X,_,_} <- Intf].
    
broadcast(Message, Intf) ->
    [X!Message|| {_,_,X}<-Intf].