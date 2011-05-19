%% @version 0.1
%% @doc A module made for handling POST requests
%% @since 2011-05-12

-module(post).
-export([handler/1]).

-include_lib("eunit/include/eunit.hrl").

%% //==================\\
%% ||EXPORTED FUNCTIONS||
%% \\==================//
%% @spec ({atom, Tuplelist}) -> Tuplelist
%% @doc Takes a post-tuple with a Tuplelist of different POST 
%% 	headers and returns a TupleList with HTTP headers and a body.
%% 	If the atom isn't 'post' an error is returned
%% @todo Implementera for more headers
%% @since 2011-05-12 | 21:00

handler({post, TupleList}) -> handlerAUX(TupleList);
handler({_,_}) -> {error, notCorrectlyTagged}.

%% //==================\\
%% ||INTERNAL FUNCTIONS||
%% \\==================//
%% @spec (HList::Tuplelist) -> String | {error_eval, Bin} | {error, Reason}
%% @doc Takes a Tuplelist with different headers and returns
%% 	a string containing a file if the path-header corresponds 
%%	to an existing file.
%%	If the file does'nt exist it checks if the path-header refers
%%	to a directory instead, if that's the case a list containing
%%	all the files and directory's in the path-directory is returned.
%%	If the path-header doesn't refer to neiher a file nor directory
%%	an error is returned with information about why there was an error.
%% @since 2011-05-12 | 21:00

handlerAUX(HList) -> 
	FileName = case lists:keysearch(filename, 1, HList) of 
		{value, {filename, FN}} ->
			FN;
		false ->
			{error, enoent}
	end,
	Path = case lists:keysearch(path, 1, HList) of 
    	{value,{path, P}} ->
		   	fm:fixPath(P);
	    false ->
			{error, enoent}
	end,
	FileContents = case lists:keysearch(file, 1, HList) of 
		{value, {file, F}} ->
			F;
		false ->
			{error, enoent}
	end,
        io:format("~n~n~n~p~n~n~n~n~n", [FileContents]),
	Boundary = case lists:keysearch(boundary, 1, HList) of 
    	{value, {boundary, B}} ->
		   	B;
	    false ->
			{error, enoent}
	end,
	io:format(Path),
	io:format("~n"),
	io:format(FileName),
	io:format("~n"),
	%%io:format([FileContents]),
	case fm:uploadFile(Path ++ FileName, FileContents) of
		ok ->
			case fm:dirHandler(Path) of
				{ok, DirList} -> 
					gen_html:postHTML (DirList, HList, Path);
				{error_eval, Bin} ->
					Bin;
				ErrorTuple ->
					ErrorTuple
			end;
		{error_eval, Bin} ->
			Bin;
		ErrorTuple ->
			ErrorTuple
	end.
	    	
	    
	    
	    
	    
	    
	    
	    
	    
	    
	    
	    
	    
	    
	    
