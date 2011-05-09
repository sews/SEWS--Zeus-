%% @version 0.1
%% @title GET module
%% @doc handling GET requests

-module(get).
-export([handler/1]).


%% @doc Handler
handler({get, Tuple_list}) -> handlerAUX(Tuple_list, []);
handler({_,_}) -> {error, notCorrectlyTagged}.

%% @doc Takes an get-tuple with TupleList of GET headers, returns a TupleList with HTTP headers and a body.
%%TODO: Implementera för fler headers
handlerAUX([],Acc) -> list_to_binary(Acc);
handlerAUX([H|T],Acc) -> 
    case H of
	{path, Path} ->
	    case fm:getFile(Path) of
		{file,File_handle} ->
		    list_to_binary(fm:getContents(File_handle));
		{dirlist, DirList} ->
		    gen_html:dirDoc(DirList);
		{error, Reason} ->
		    error_mod:handler(Reason)
	    end;
	%% {host, Host} -> tbi.	   
	Any -> handlerAUX(T, Acc)
    end.
	    


