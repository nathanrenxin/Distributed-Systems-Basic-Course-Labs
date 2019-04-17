-module(dijkstra).
-export([update/4]).
-export([iterate/3]).
-export([table/2]).
-export([route/2]).


entry(Node, Sorted) -> 
    case lists:keyfind(Node, 1, Sorted) of
        false -> 0; %0 is used so that it will never be updated, which means new entry will not be added if it does not exist
        {Node, Length, _} -> Length
    end.

replace(Node, N, Gateway, Sorted) ->
    lists:keysort(2, lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway})).
    
update(Node, N, Gateway, Sorted) ->
    Length = entry (Node, Sorted),
    case Length of
        inf -> replace(Node, N, Gateway, Sorted);
        _ -> 
            case N-Length<0 of
                true -> replace(Node, N, Gateway, Sorted);
                false -> Sorted
            end
    end.
    
iterate([], _Map, Table) -> 
    Table;
iterate([{_,inf,_}|_], _Map, Table) -> 
    Table;  
iterate([{Node,Length,Gateway}|T], Map, Table) ->   
    ReachableNode = map:reachable(Node,Map),
    ToBeUpdated = [{X, Length+1, Gateway}|| X <- ReachableNode],
    NewT = lists:foldl(fun({X,Y,Z}, Sorted) -> update(X,Y,Z, Sorted) end, T, ToBeUpdated),
    iterate(NewT, Map, Table++[{Node,Gateway}]).
    
table(Gateways, Map) -> 
    AllNodes = map:all_nodes(Map),
    AllLinks = lists:foldl(fun(X, Links) -> lists:delete(X, Links) end, AllNodes, Gateways),
    Sorted = [{X,0,X}||X<-Gateways] ++ [{X,inf,unknown}||X<-AllLinks],
    iterate(Sorted, Map, []).
    
route(Node, Table) -> 
      case lists:keyfind(Node, 1, Table) of
          false -> notfound;
          {Node, Gateway} -> {ok, Gateway}
      end.
    