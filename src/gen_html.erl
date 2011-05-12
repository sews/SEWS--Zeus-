%% @version 0.2
%% @doc A module made for generating html code
%% @since 12.05.11

-module(gen_html).
-compile([dirDoc/2]).

-include_lib("eunit/include/eunit.hrl").

%% //==================\\
%% ||INTERNAL FUNCTIONS||
%% \\==================//

%% dirDocAux(DirList, Path, Html)
%% @hidden@spec (DirList::list, Path::list, Html::list) -> String::list
%% @hidden@doc Generates a simple UI to the file or directory at DirList.
%%             The path to the current folder is contained at all points of execution
%%             in the Path variable. The html code is saved in the variable Html.
%%             Returns a string containing a html document.
%% @hidden@private Removed Host as a variable
%% @since 12.05.11

dirDocAux(DirList, Path, [])->
    dirDocAux(DirList, Path, "<html><head><title>Index of " ++ Path ++ "</title></head><body><h1>Index of " ++ Path ++ "</h1><hr>");
dirDocAux([File|FileTail], Path, Html) ->
    IsDir = filelib:is_dir(Path ++ File),
    case IsDir of
	 true -> %% File refers to a directory
	    dirDocAux(FileTail, Path, Html ++ "[Dir] <a href='" ++ Path ++ File ++ "'>" ++ File ++ "</a>" ++ "<br />");
	false -> %% File refers to a file
	    dirDocAux(FileTail, Path, Html ++ "[File] <a href='" ++ Path ++ File ++ "'>" ++ File ++ "</a>" ++ "<br />")
    end;
dirDocAux([], _, Html) ->
    Html ++ "<hr></body></html>".
    
%% //==================\\
%% ||EXPORTED FUNCTIONS||
%% \\==================//

%% @doc Returns a string containing HTML code to be used in a webserver,
%%      generated from files in DirList and headers in HList
%% @spec dirDoc(DirList::list, Hlist::list) -> String::list
%% @since 12.05.11

dirDoc(DirList, HList)-> 

%% Get path so requested file from HList
    Path = case lists:keysearch(path, 1, HList) of 
    	{value,{path, P}} ->
		   	P;
	       	false ->
		   	error_mod:handler(nopath)
	   	end,
    dirDocAux(DirList, Path, Host,[]).
    
    
%% TEST CASES

dirDoc_test() ->
	?assertEqual("<html><head><title>hej</title></head><body></body></html>", dirDoc([], [{path, "hej"},{host, ""}])).


