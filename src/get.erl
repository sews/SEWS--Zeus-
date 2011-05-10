%% @version 0.1
%% @title GET module
%% @doc handling GET requests

-module(get).
-export([handler/1]).

-include_lib("eunit/include/eunit.hrl").

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
			{ok, File_handle} ->
				list_to_binary(fm:getContents(File_handle));
			{error, eisdir} ->
				case fm:dirHandler(Path) of
					{ok, DirList} -> 
						gen_html:dirDoc(DirList, HList);
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
    
%%handler_test() ->	%% wut??????
%%	?_assert({ok, Reason} =:= handler({get, [{path, "/home/dennisrosen/little alex on horse"}]})).



