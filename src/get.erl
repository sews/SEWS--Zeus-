%% @author Alex
%% @copyright © 2011
%% @version 0.1
%% @title GET module
%% @doc handling GET requests

-module(get).
-export([handler/1]).


%% @doc Handler
handler({get, TL}) -> handlerAUX(TL);
handler({_,_}) -> {error, notCorrectlyTagged}.

%% @doc Takes an get-tupe with TupleList of GET headers, returns a TupleList with HTTP headers and a body.

handlerAUX([],Acc) -> list_to_binary(Acc);
handlerAUX([H|L],Acc) -> 
    case H of
	{path, Path} ->
	    case filemanager:getFile(Path) of
		{ok, File, Info} -> tbi;  %% binary data to browser ? 
		{error, Reason} -> {error,Reason}
	    end;
	{host, Host} ->
	    case 
	        {
	Any -> handleAUX(L)
    end.
	    


