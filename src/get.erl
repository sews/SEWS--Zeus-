%% @author Alex
%% @copyright © 2011
%% @version 0.1
%% @title GET module
%% @doc handling GET requests

-module(get).
-export([handler/1]).


%% @doc Handler
handler({get, TL}) -> handler'(TL);
handler({_, TupleList}) -> {error, notCorrectlyTagged}.

%% @doc Takes an get-tupe with TupleList of GET headers, returns a TupleList with HTTP headers and a body.
handler'([]) -> ok;
handler'([{path, Path}|L]) ->
	case filemanager:getFile(Path) of
		{ok, File, Info} -> tbi;
		{error, Reason} -> tbi
	end;
handler'([H|L]) -> handler'(L).

