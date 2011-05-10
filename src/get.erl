%% @version 0.1
%% @title GET module
%% @doc handling GET requests

-module(get).
-export([handler/1]).


%% @doc Handler
handler({get, Tuple_list}) -> handlerAUX(Tuple_list);
handler({_,_}) -> {error, notCorrectlyTagged}.

%% @doc Takes an get-tuple with TupleList of GET headers, returns a TupleList with HTTP headers and a body.
%%TODO: Implementera för fler headers
						%handlerAUX([]) -> list_to_binary(Acc);
handlerAUX(HList) -> 
    case lists:keysearch(path, 1, HList) of
	{value,{path, Path}} ->
	    case fm:getFile(Path) of
		{ok ,File_handle} ->
		    list_to_binary(fm:getContents(File_handle));
		{error, eisdir} ->
		    case fm:dirHandler(Path) of
			{ok, DirList} -> 
			    gen_html:dirDoc(DirList, HList);
			ErrorBin ->
			    ErrorBin
		    end;
		{error, Reason} ->
		    error_mod:handler(Reason)
	    end;
	false ->
	    error_mod:handler(nopath);
	Any -> io:format("~n~p~n", [Any])
    end.




