-module(error_mod).
-export([handler/1]).
%% @version 0.1
%% handler(Reason)
%% @spec atom -> string() 
%% @doc Handle a error Reason and return the right error message for the given error.
handler(Reason) ->
	{error_eval, list_to_binary("<html><head><title>Error</title></head><body>" ++ 
	case Reason of
		enoent ->
			"404 - File not found error";
		eaccess ->
			"1337 - Access denied suckers";
		_ ->
			"1000 - " ++ atom_to_list(Reason) ++ " error found"
	end ++ "</body></html>")}.
