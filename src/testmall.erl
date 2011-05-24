%% Mall för tester med exempel-funktioner och -tester


-module(testmall).
-export([]).

-include_lib("eunit/include/eunit.hrl").

new() -> %%Returns a new key-value store.
    [].
    
%% @deprecated
%% size(KV) -> %%Returns the number of key-value pairs in the key-value store KV.
%%    length(KV).

add(KV, Key, Value) -> %%Add the Key together with Value to key-value store KV.
    case (lists:keysearch(Key, 1, KV)) of
	{value, _} ->
	    lists:keyreplace(Key, 1, KV, {Key, Value});
	false ->
	    [{Key, Value} | KV]
    end.
    
value(KV, Key) -> %%Returns the value for key Key, or false if key does not exist
    case (lists:keysearch(Key, 1, KV)) of
	{value, Tup} ->
	    Tup;
	false ->
	    false
    end.

key(KV, Value) -> %%Returns the key stored toghether with Value. What if Valueis not in KV? What if there are more than one match for Value in the key-value store?
    case (lists:keysearch(Value, 2, KV)) of
	{value, Tup} ->
	    Tup;
	false ->
	    false
    end.

delete(KV, Key) -> %%Delete a pair given the Key.
    lists:keydelete(Key, 1, KV). 

   %%%%%%% Eunit test cases  %%%%%%%

%% EUnit adds the fifo:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by fifo:test()

f1() ->
    add(add(add(new(), sverige, stockholm), finland, helsingfors), "norge", "oslo").

size_test() ->
    ?_assertEqual(3, kv:size(f1())).

new_test() -> 
    ?_assertMatch(0, kv:size(new())).

value_test() ->
    ?_assertEqual(false, value(new(), key)).
    
key_test() ->
    ?_assertEqual(false, key(new(), value)).
    
add_test_() ->
    ?_assertMatch([{sverige, stockholm}], add(new(), sverige, stockholm)).

delete_test() ->
    F = f1(),
    F2 = delete(F, sverige),
    F3 = delete(F2, finland),
    F4 = delete(F3, "norge"),
    
    [?_assertMatch(3, kv:size(F)),
    ?_assertMatch(2, kv:size(F2)),
    ?_assertMatch(1, kv:size(F3)),
    ?_assertMatch(0, kv:size(F4))].
    
