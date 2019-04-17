-module(vect).
-export([zero/0, inc/2, merge/2, leq/2, clock/0, update/3, safe/2]).

zero() -> [].

inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, T} -> lists:keyreplace(Name, 1, Time, {Name, T+1});
        false -> [{Name,1}|Time]
    end.
    
merge([], Time) ->
    Time;
merge([{Name, Ti}|Rest], Time) ->
    %io:format("vect: merge ~p ~p~n", [Name, Time]),
    case lists:keyfind(Name, 1, Time) of 
        {Name, Tj} -> merge(Rest, lists:keyreplace(Name, 1, Time, {Name, max(Ti,Tj)}));
        false -> merge(Rest, [{Name, Ti}|Time])
    end.
    
    
leq([], _) -> true;
leq([{Name, Ti}|Rest],Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            if
                Ti =< Tj ->
                    leq(Rest,Time);
                true ->
                    false
            end;
        false -> false
    end.
    
clock() -> [].

update(Node, Time, Clock) -> 
    {Node, Counter} = lists:keyfind(Node, 1, Time),
    case lists:keyfind(Node, 1, Clock) of
        {Node, _} -> 
            lists:keyreplace(Node, 1, Clock, {Node, Counter});
        false -> 
            [{Node, Counter}|Clock]
    end.
%update(Node, Time, Clock) -> 
%    AllNodes = [X||{X,_}<-Time],
%    do_update(Node, Time, AllNodes, Clock).
%
%do_update(_Node, _Time, [], Clock) -> Clock;
%do_update(Node, Time, [Node|T], Clock) ->
%    {Node, Counter} = lists:keyfind(Node, 1, Time),
%    case lists:keyfind(Node, 1, Clock) of
%        {Node, _} -> 
%            NewC=lists:keyreplace(Node, 1, Clock, {Node, Counter}),
%            do_update(Node,Time,T,NewC);
%        false -> 
%            NewC=[{Node, Counter}|Clock],
%            do_update(Node,Time,T,NewC)
%    end;
%
%do_update(Node, Time, [Node1|T], Clock) ->
%    case lists:keyfind(Node1, 1, Clock) of
%        {Node1, _} -> 
%            do_update(Node,Time,T,Clock);
%        false -> 
%            NewC=[{Node1, 0}|Clock],
%            do_update(Node,Time,T,NewC)
%    end.

%update(Node, Time, Clock) -> 
%    AllNodes = [X||{X,_}<-Time],
%    do_update(Node, Time, AllNodes, Clock).
%
%do_update(_Node, _Time, [], Clock) -> Clock;
%do_update(Node, Time, [Node|T], Clock) ->
%    case lists:keyfind(Node, 1, Clock) of
%        {Node, _} -> 
%            NewC=lists:keyreplace(Node, 1, Clock, {Node, Time}),
%            do_update(Node,Time,T,NewC);
%        false -> 
%            NewC=[{Node, Time}|Clock],
%            do_update(Node,Time,T,NewC)
%    end;
%
%do_update(Node, Time, [Node1|T], Clock) ->
%    case lists:keyfind(Node1, 1, Clock) of
%        {Node1, _} -> 
%            do_update(Node,Time,T,Clock);
%        false -> 
%            NewC=[{Node1, []}|Clock],
%            do_update(Node,Time,T,NewC)
%    end.

safe(Time,Clock) ->leq(Time, Clock).
    
%safe(_, []) -> true;
%safe(Time,[{_,Clock}|Rest]) ->
%    case leq(Time, Clock) of
%        true ->
%            safe(Time,Rest);
%        false ->
%            false
%    end.