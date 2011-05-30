%% @author Grupp 6
%% @doc Handles Dynamic Erlang script files

-module(dynerl).
-export([match/1]).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


match(String) ->
	Start = string:str(String, "<?erl"),
	End = string:str(String, "?>"),
	case ((Start > 0) and (End > 0)) of
	    true ->
		case eval(string:substr(String, Start + 5, End - Start - 5)) of 
		    {error, Reason} ->
			%% io:format("~n~p~n", [String]),
			String;
		    Value ->
			NewString = string:substr(String, 1, Start - 1) ++ Value ++ match(string:substr(String, End + 2)),
			%% io:format("~n~p~n", [NewString]),
			NewString
		end;
	    _ ->
		%% io:format("~n~p~n", [String]),
		String
	end.	
       		    
eval(String) ->
    {ok,ErlTokens,_} = erl_scan:string(String),
    %% io:format("ErlTokens are ~p~n",[ErlTokens]),
    case erl_parse:parse_exprs(ErlTokens) of
    	{ok,ErlAbsForm} -> 
	    %% io:format("ErlAbsForm are ~p~n",[ErlAbsForm]),
	    %Bindings=erl_eval:add_binding('A',20,erl_eval:new_bindings()),
	    %NewBindings=erl_eval:add_binding('B',45,Bindings),
	    %io:format("The bindings are ~p~n",[erl_eval:bindings(NewBindings)]),
	    %io:format("Going into erl_eval:exprs~n",[]),
			{value,Value,_} = erl_eval:exprs(ErlAbsForm, erl_eval:new_bindings()),
			lists:flatten(io_lib:format("~p", [Value]));
	    %% io:format("Value is ~p~n",[Value]),

		{error, Reason} -> String
    end.

f1() ->
	%T = "A = 1 + 1. B = 7*7. A+B.".
	T = "<html>
	<head>
	</head>
	<body>

        <h1>Tjena!</h1>

	<?erl
	A = 1 + 1, 
	B = A + 3, 
	A * B.
	?>

        <h2>Tjada!</h2>

	</body>
	</html>
	".

eval_test() ->
	Var = f1(),
	[?_assertEqual(10, dynerl:match(Var))].
