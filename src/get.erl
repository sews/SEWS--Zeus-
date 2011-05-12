%% @version 0.1
%% @doc A module made for handling GET requests

-module(get).
-export([handler/1]).

-include_lib("eunit/include/eunit.hrl").

%% @spec ({atom(), Tuple_list}) -> Tuple_list
%% @doc Takes a get-tuple with TupleList of GET headers, returns a TupleList with HTTP headers and a body.
%% @todo Implementera for more headers

handler({get, Tuple_list}) -> handlerAUX(Tuple_list);
handler({_,_}) -> {error, notCorrectlyTagged}.

handlerAUX(HList) -> 
    case lists:keysearch(path, 1, HList) of
	{value,{path, Path}} ->
	    case fm:getFile(Path) of
			{ok, File_handle} ->
				list_to_binary(fm:getContents(File_handle));
			{error, eisdir} ->
				case fm:getFile(Path ++ "index.html") of
					{ok, File_handle} ->
						list_to_binary(fm:getContents(File_handle));
					{error, enoent} ->	
						case fm:dirHandler(Path) of
							{ok, DirList} -> 
								gen_html:dirDoc(DirList, HList);
							{error_eval, Bin} ->
								Bin
						end;
					{error_eval, Bin} ->
						Bin
				end;
			{error, Reason} ->
				{error_eval, Bin} = error_mod:handler(Reason),
				Bin;
			{error_eval, Bin} ->
				Bin
		end;
	false ->
	    {error_eval, Bin} = error_mod:handler(nopath),
	    Bin;
	Any -> io:format("~n~p~n", [Any])
    end.
    



