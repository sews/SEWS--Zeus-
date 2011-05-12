-module(gen_html).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


%% INTERNAL FUNCTIONS

%% @todo: Optimize
dirDocAux(DirList, Path, Host, [])->
    dirDocAux(DirList, Path, Host, "<html><head><title>Index of " ++ Path ++ "</title></head><body><h1>Index of " ++ Path ++ "</h1><hr>");
dirDocAux([File|FileTail], Path, Host, Html) ->
    IsDir = filelib:is_dir(Path ++ File),
    case IsDir of
	 true -> %% File refers to a directory
	    dirDocAux(FileTail, Path, Host, Html ++ "[Dir] <a href='" ++ Path ++ File ++ "'>" ++ File ++ "</a>" ++ "<br />");
	false -> %% File refers to a file
	    dirDocAux(FileTail, Path, Host, Html ++ "[File] <a href='" ++ Path ++ File ++ "'>" ++ File ++ "</a>" ++ "<br />")
    end;
dirDocAux([], _, _, Html) ->
    Html ++ "<hr></body></html>".
    

%% EXPORTED FUNCTIONS

%% @doc Returns a string containing HTML code to be used in a webserver, generated from files in DirList and headers in HList
%% @spec dirDoc(DirList::list, Hlist::list) -> String::list

dirDoc(DirList, HList)-> 
    Path = case lists:keysearch(path, 1, HList) of
    	{value,{path, P}} ->
		   	P;
	       	false ->
		   	error_mod:handler(nopath)
	   	end,
    Host = case lists:keysearch(host, 1, HList) of	%% keysearch Path and Host for easy and fast access during HTML generation
    	{value,{host, H}} ->
			H;
	       	false ->
		   	error_mod:handler(nohost)
	   	end,
    dirDocAux(DirList, Path, Host,[]).
    
    
%% TEST CASES

dirDoc_test() ->
	?assertEqual("<html><head><title>hej</title></head><body></body></html>", dirDoc([], [{path, "hej"},{host, ""}])).


