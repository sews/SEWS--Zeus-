-module(gen_html).
-compile(export_all).


dirDoc(DirList)-> 
    dirDocAux(DirList,[]).
    
%%Todo: title and stuff and optimize
dirDocAux(DirList, [])->
    dirDocAux(DirList,"<HTML><HEAD><Title></title></HEAD><BODY>");
dirDocAux([H|Rest],Html) ->
    dirDocAux(Rest,Html ++ H);
dirDocAux([],Html) ->
    Html ++ "</BODY></HTML>".
