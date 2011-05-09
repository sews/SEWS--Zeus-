-module(error_mod).
-export([handler/1]).

handler(Reason) ->
	{ok, Reason}.
