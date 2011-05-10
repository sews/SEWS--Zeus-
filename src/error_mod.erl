-module(error_mod).
-export([handler/1]).

handler(Reason) ->
	case Reason of
		Reason ->
			{error_eval, list_to_binary("hej")}
	end.
