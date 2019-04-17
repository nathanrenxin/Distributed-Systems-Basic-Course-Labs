-module(hist).
-export([new/1]).
-export([update/3]).

new(Name) -> [{Name, inf}].

update(Node, N, History) -> 
    case lists:keyfind(Node, 1, History) of
        false -> {new, History ++ [{Node,N}]};
        {Node, inf} -> old;
        {Node, Latest} ->
            case N-Latest > 0 of
                true -> {new, lists:keyreplace(Node, 1, History, {Node, N})};
                false -> old
            end
    end.