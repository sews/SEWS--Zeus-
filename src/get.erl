%% @author Grupp 6
%% @version 0.1
%% @doc A module made for handling GET requests
%% @since 12.05.11

-module(get).
-export([handler/1]).

-include_lib("eunit/include/eunit.hrl").

%% //==================\\
%% ||EXPORTED FUNCTIONS||
%% \\==================//
%% @spec ({atom, tuplelist}) -> tuplelist
%% @doc Takes a get-tuple with a Tuplelist of different GET 
%% 	headers and returns a TupleList with HTTP headers and a body.
%% 	If the atom isn't 'get' an error gets returned
%% @todo Implementera for more headers
%% @since 12.05.11 

handler({get, Tuple_list}) -> handlerAUX(Tuple_list);
handler({_,_}) -> {error, notCorrectlyTagged}.

%% //==================\\
%% ||INTERNAL FUNCTIONS||
%% \\==================//
%% @spec (HList::tuplelist) -> string | {error_eval, Bin} | {error, Reason}
%% @doc Takes a Tuplelist with different headers and returns
%% 	a string containing a file if the path-header corresponds 
%%	to an existing file.
%%	If the file does'nt exist it checks if the path-header refers
%%	to a directory instead, if that's the case a list containing
%%	all the files and directory's in the path-directory is returned.
%%	If the path-header doesn't refer to neiher a file nor directory
%%	an error is returned with information about why there was an error.
%% @since 12.05.11 

handlerAUX(HList) -> 
    case lists:keysearch(path, 1, HList) of
	{value, {path, Path}} ->
	    case fm:getFile(Path) of
			{ok, File_handle} -> 
				list_to_binary(fm:getContents(File_handle));
			{error, eisdir} ->
				case fm:getFile(Path ++ "index.html") of
					{ok, File_handle} -> %% 
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

%% //============\\
%% || TEST CASES ||
%% \\============//

%%get_test(
%%  ?_assertEqual(
