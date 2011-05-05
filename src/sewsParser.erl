-module(sewsParser).
-export([parse/1]).

parse(Input)->
    [H|T] = string:tokens(Input," "),
    case H of
        "GET " -> parseGET(T);
        "POST" -> parsePOST(T);
        Any -> {error, notGETorPOST}
    end.

parseGET([H|T], []) ->
    case H of
       "/" -> parseGET(T,[{path,"/index.html"}]);
        [$/|Rest]-> parseGET(T,[{path,H}]);
        Any -> {error, badly_formed} % Kan vara badly formed även om den inte går in här såklart
    end;
parseGET(L,Parsed_list) ->
    [H|T] = Next_header = get_next_header(L),
    case H of
	"Host:" ->
	    [Host|Rest] = T,
	    parseGET(Next_header,[{host, Host}|Parsed_list]);
	"Connection:" ->
	    [Connection_type|Rest] = T,
	    parseGET(Next_header,[{con_type,Connection_type}|Parsed_list]);
	Any -> parseGET(Next_header, Parsed_list) %%  Ignorerar alla andra headers än så länge
						% TODO?: Implementera för fler?
    end;
parseGET() ->

	    

parsePOST([H|T])->
    todo.