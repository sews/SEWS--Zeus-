-module(error_mod).
-export([handler/1]).

handler(Reason) ->
	{error_eval, list_to_binary("<html><head><title>Error</title></head><body>" ++ 
	case Reason of
		enoent ->
			"404 - File not found error";
		eaccess ->
			"1337 - Access denied suckers";
		_ ->
			"1000 - Random error found"
	end ++ "</body></html>")}.
