-module(map).
-export([new/0]).
-export([update/3]).
-export([reachable/2]).
-export([all_nodes/1]).

new() -> [].

update(Node, Links, Map) ->
    case lists:keyfind(Node, 1, Map) of
        false -> Map ++ [{Node, Links}];
        {Node, _Oldlinks} -> lists:keyreplace(Node, 1, Map, {Node, Links})
    end.

reachable(Node, Map) -> 
    case lists:keyfind(Node, 1, Map) of
        false -> [];
        {Node, Links} -> Links
    end.

all_nodes(Map) -> 
    AllNodes = lists:foldl(fun(X, Sum) -> get_all_nodes(X) ++  Sum end, [], Map),
    remove_duplication(AllNodes).
    
    
get_all_nodes({Node, Links}) -> [Node] ++ Links.

remove_duplication(AllNodes) ->
    remove_duplication(AllNodes, []).

remove_duplication([], AllNodes) ->
    AllNodes;
remove_duplication([Node|T], AllNodes) ->
    case find_node(Node, AllNodes) of
        true -> remove_duplication(T, AllNodes);
        false -> remove_duplication(T, AllNodes++ [Node])
    end.

find_node(_Node, []) -> false;  
find_node(Node, [Node|_]) -> true;
find_node(Node, [_| T]) -> find_node(Node, T).