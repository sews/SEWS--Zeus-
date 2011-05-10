-module(gen_html).
-compile(export_all).


dirDoc(DirList, HList)-> 
    Path = case lists:keysearch(path, 1, HList) of
	       {value,{path, P}} ->
		   P;
	       false ->
		   error_mod:handler(nopath)
	   end,
    Host = case lists:keysearch(host, 1, HList) of
	       {value,{host, H}} ->
		   H;
	       false ->
		   error_mod:handler(nohost)
	   end,
    dirDocAux(DirList, Path, Host,[]).

%% Todo: Make title show the path, optimize, fix the links so they work properly and sort them first according to type(directories/files) and then in lexographical order
dirDocAux(DirList, Path, Host, [])->
    dirDocAux(DirList, Path, Host, "<html><head><title>" ++ Path ++ "</title></head><body>");
dirDocAux([File|FileTail], Path, Host, Html) ->
    IsDir = filelib:is_dir(Path ++ File),
    case IsDir of
	 true -> %% File refers to a directory
	    dirDocAux(FileTail, Path, Host, Html ++ "Dir:<A HREF='" ++ Path ++ File ++ "'>" ++ File ++ "</A>" ++ "<BR>");
	false -> %% File refers to a file
	    dirDocAux(FileTail, Path, Host, Html ++ "File:<A HREF='" ++ Path ++ File ++ "'>" ++ File ++ "</A>" ++ "<BR>")
    end;
dirDocAux([], _, _, Html) ->
    Html ++ "</BODY></HTML>".

