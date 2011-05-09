-module(gen_html).
-compile(export_all).


dirDoc(DirList, HList)-> 
	Path = case lists:key_search(path, 1, HList) of
		{value, P} ->
			P;
		false ->
			error_mod:handler(nopath)
	end,
	Host = case lists:key_search(host, 1, HList) of
		{value, H} ->
			H;
		false ->
			error_mod:handler(nohost)
	end,
    dirDocAux(DirList, Path, Host,[]).
    
%%Todo: title and stuff and optimize
dirDocAux(DirList, Path, Host, [])->
    dirDocAux(DirList, Path, Host, "<HTML><HEAD><Title></title></HEAD><BODY>");
dirDocAux([H|Rest], Path, Host, Html) ->
    dirDocAux(Rest, Path, Host, Html ++ "<A HREF='" ++ Path ++ "'>" ++ H ++ "</A>" ++ "<BR>");
dirDocAux([], _, _, Html) ->
    Html ++ "</BODY></HTML>".
