%% @author Grupp 6
%% @doc Handles Dynamic Erlang script files

-module(dynerl).
-export([eval/1]).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

eval(String) ->
    {ok,ErlTokens,_} = erl_scan:string(String),
    io:format("ErlTokens are ~p~n",[ErlTokens]),

    {ok,ErlAbsForm} = erl_parse:parse_exprs(ErlTokens),
    io:format("ErlAbsForm are ~p~n",[ErlAbsForm]),

    %Bindings=erl_eval:add_binding('A',20,erl_eval:new_bindings()),
    %NewBindings=erl_eval:add_binding('B',45,Bindings),
    %io:format("The bindings are ~p~n",[erl_eval:bindings(NewBindings)]),

    %io:format("Going into erl_eval:exprs~n",[]),
    {value,Value,_} = erl_eval:exprs(ErlAbsForm, erl_eval:new_bindings()),
    %io:format("Value is ~p~n",[Value]).
    Value.

f1() ->
	%T = "A = 1 + 1. B = 7*7. A+B.".
	T = "A = 1 + 1, B = A + 3, A * B.".

eval_test() ->
	Var = f1(),
	[?_assertMatch(10, dynerl:eval(Var))].
	