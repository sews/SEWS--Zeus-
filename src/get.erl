%% @version 0.1
%% @title GET module
%% @doc handling GET requests

-module(get).
-export([handler/1]).


%% @doc Handler
handler({get, TL}) -> handlerAUX(TL, []);
handler({_,_}) -> {error, notCorrectlyTagged}.

%% @doc Takes an get-tuple with TupleList of GET headers, returns a TupleList with HTTP headers and a body.

handlerAUX([],Acc) -> list_to_binary(Acc);
handlerAUX([H|L],Acc) -> 
    case H of
	{path, Path} ->
	    case fm:getFile(Path) of
		{ok, File, Info} -> handlerAUX(L, [File|Acc]);
		%% Handle the error if the file wasn't found
		{error, Reason} -> tbi.
	    end;
	%% {host, Host} -> tbi.
	   
	Any -> handlerAUX(L, Acc)
    end.
	    


