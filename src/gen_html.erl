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
    DirFileList = fm:filesAndDirs(DirList,Path),
    OrderingFun = fun({Atom1,File1},{Atom2,File2})-> 
		      File1Low = string:to_lower(File1),
		      File2Low = string:to_lower(File2),
		      if 
			  Atom2 == isdir, Atom1 == isdir, File1Low > File2Low -> false;
			  Atom2 == isfile,Atom1 == isfile, File1Low > File2Low -> false;
			  Atom2 == isdir,Atom1 == isfile -> false;
			  true -> true
		      end
		  end,
    SortedDirList = lists:sort(OrderingFun,DirFileList),
    FinishedDirList = lists:map(fun({_,File}) -> File end, SortedDirList),
    dirDocAux(FinishedDirList, Path, Host,[]).

%% Todo: Make title show the path, optimize, fix the links so they work properly and sort them first according to type(directories/files) and then in lexographical order
dirDocAux(DirList, Path, Host, [])->
    dirDocAux(DirList, Path, Host, "<HTML><HEAD><Title></title></HEAD><BODY>");
dirDocAux([File|FileTail], Path, Host, Html) ->
    IsDir = filelib:is_dir(Path ++ File),
    if 
	IsDir -> %% File refers to a directory
	    dirDocAux(FileTail, Path, Host, Html ++ "Dir:<A HREF='" ++ Path ++ "'>" ++ File ++ "</A>" ++ "<BR>");
	true -> %% File refers to a file
	    dirDocAux(FileTail, Path, Host, Html ++ "File:<A HREF='" ++ Path ++ "'>" ++ File ++ "</A>" ++ "<BR>")
    end;
dirDocAux([], _, _, Html) ->
    Html ++ "</BODY></HTML>".

