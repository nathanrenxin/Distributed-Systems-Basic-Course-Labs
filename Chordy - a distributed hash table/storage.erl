-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).


create() -> [].

add(Key, Value, Store) -> lists:keysort(1, Store ++ [{Key, Value}]).

lookup(Key, Store) -> lists:keyfind(Key, 1, Store).

split(From, To, Store) -> lists:partition(fun({Key, _Value}) -> key:between(Key, From, To) end, Store).

merge(Entries, Store) -> lists:keysort(1, Store ++ Entries).