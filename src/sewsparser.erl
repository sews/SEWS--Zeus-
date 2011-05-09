
%% @doc Sews Parser: Parses the reqests to a format suitable to erlang.  

-module(sewsparser).
%-export([parse/1]).
-compile(export_all).


%% For testing purposes only
string()->
    "GET /favicon.ico HTTP/1.1\r\nHost: 127.0.0.1:8888\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:2.0.1) Gecko/20100101 Firefox/4.0.1\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-us,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 115\r\nConnection: keep-alive\r\n\r\n".



%%%%%%%%%%%%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%
%% @hidden@doc Parser for the html requests.
%% @hidden@spec parse(Input::String) -> Tuple_list
%%  Pre: A POST or GET request in the standard format
%%  Post: A tuple {type_atom(), Tuple_list} where Tuple_list consists of tuples of the format {header_atom(), Data} or a tuple {error, Reason}
%% possible tuples: {path, Path}, {host, Host}, {connection, Connection}
%% S-E: None 
%% Ex: sewsparser:parse("GET /"). -> {get,[{path,"/index.html"}]}

parse(Input)->
    L = string:tokens(Input,"\n"),
    [H|T] = lists:map(fun(X) -> string:tokens(X," \r\007") end,L),
    [H1|Rest] = H,
    case H1 of
        "GET" -> parseGET([Rest|T],[]);
        "POST" -> parsePOST([Rest|T],[]);
        Any -> {error, notGETorPOST}
    end.


%%%%%%%%%%%%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%

%% @doc Parse out the keywords and the relevant data from the keywords
%% @spec parseGET(String::list,Tuple::list) -> Tuple::list
%% Pre: A correctly formated GET request starting with the path
%% Post: A tupple of the format {get, Tuple_list} or {error, Reason}, where Tuple_list consists of tuples of the format {header_atom(), Data}
%% S-E None
%% TODO: Implementera för fler headers 
parseGET([[]],Parsed_list) ->
    {get, Parsed_list};
parseGET([],Parsed_list)->
    {get, Parsed_list};
parseGET([[H|T2]|T], []) ->
    case H of
        [$/|Rest]-> parseGET(T,[{path,H}]);
        Any -> {error, badly_formed} 
    end;
parseGET([[H|T2]|T],Parsed_list) ->
	    case H of
		"Host:" ->
		    [Host|Rest] = T2,
		    parseGET(T,[{host, Host}|Parsed_list]);
		"Connection:" ->
		    [Connection_type|Rest] = T2,
		    parseGET(T,[{connection,Connection_type}|Parsed_list]);  %% kanske ta med mer info
		"Accept-Language:" ->
		    parseGET(T,[{accept_language,T2}|Parsed_list]);
		"User-Agent:" -> 
		    parseGET(T,[{user_agent,T2}|Parsed_list]);
		"Accept-Encoding:" ->
		    parseGET(T,[{accept_encoding,T2}|Parsed_list]);
		"Accept-Charset:" ->
		    parseGET(T,[{accept_charset,T2}|Parsed_list]);
		"Keep-Alive:" ->
		    parseGET(T,[{keep_alive,T2}|Parsed_list]);
		Any -> parseGET(T, Parsed_list)
    end.


%% Pre: A correctly formated POST request starting with the path
%% Post: A tupple of the format {post, Tuple_list} or {error, Reason}, where Tuple_list consists of tuples of the format {header_atom(), Data}
%% S-E None	    

%% TODO: Implement parsePOST
parsePOST([H|T],[])->
    todo.
