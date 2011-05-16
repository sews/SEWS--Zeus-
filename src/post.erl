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
			error_mod:handler(nofilename)
	end,
	Path = case lists:keysearch(path, 1, HList) of 
    	{value,{path, P}} ->
		   	P;
	    false ->
			error_mod:handler(nopath)
	end,
	FileContents = case lists:keysearch(file, 1, HList) of 
		{value, {file, F}} ->
			F;
		false ->
			error_mod:handler(nofile)
	end,
	io:format(Path),
	io:format("~n"),
	io:format(FileName),
	io:format("~n"),
	io:format([FileContents]),
	fm:uploadFile(fm:fixPath(Path) ++ FileName, FileContents).
	    	
	    
	    
	    
	    
	    
	    
	    
	    
	    
	    
	    
	    
	    
	    
