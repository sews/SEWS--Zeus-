%% @version 0.1
%% @doc Sews Parser: Parses the requests to a format suitable to erlang.  
-module(sewsparser).
%-export([parse/1]).
-compile(export_all).

-define(SERVER_ROOT,"/www/var/").

%% For testing purposes only
string()->
    "GET /home/dennisrosen/big alex on horse.txt HTTP/1.1\r\nHost: 127.0.0.1:8888\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:2.0.1) Gecko/20100101 Firefox/4.0.1\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-us,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 115\r\nConnection: keep-alive\r\n\r\n".



%%%%%%%%%%%%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%
%% @spec parse(Input::String) -> Tuple_list
%% @doc Parser for the html requests.
%%  Pre: A POST or GET request in the standard format
%%  Post: A tuple {type_atom(), Tuple_list} where Tuple_list consists of tuples of the format {header_atom(), Data} or a tuple {error, Reason}
%% possible tuples: {path, Path}, {host, Host}, {connection, Connection}
%% S-E: None 
%% Ex: sewsparser:parse("GET /"). -> {get,[{path,"/index.html"}]}

parse(Input)->
    L = string:tokens(Input,"\n"),
    [H|T] = lists:map(fun(X) -> string:tokens(X,"%20 \r\007") end,L),
    [H1|Rest] = H,
    case H1 of
        "GET" -> parseGET([Rest|T],[]);
        "POST" -> parsePOST([Rest|T],[]);
        Any -> {error, notGETorPOST}
    end.


%%%%%%%%%%%%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%


%% @doc Returns a modified version of String with every occurence of "%20" substrings replaced with blankspaces
%% @spec stripShit(String::list) -> String::list

stripShit ([]) -> [];
stripShit ([H1]) -> [H1];
stripShit ([H1, H2]) -> [H1, H2];
stripShit ([H1,H2,H3 | Rest]) ->
	case [H1, H2, H3] of
		"%20" ->
			" " ++ stripShit(Rest);
		 _ ->
			[H1 | stripShit([H2,H3 | Rest])]
	end.
	

%% @spec parseGET(String::list,Tuple::list) -> Tuple::list
%% @todo Implementera för fler headers 
%% @doc Parse out the keywords and the relevant data from the keywords
%% Pre: A correctly formated GET request starting with the path
%% Post: A tupple of the format {get, Tuple_list} or {error, Reason}, where Tuple_list consists of tuples of the format {header_atom(), Data}
%% S-E None

parseGET([[]],Parsed_list) ->
    {get, Parsed_list};
parseGET([],Parsed_list)->
    {get, Parsed_list};
parseGET([H|T], []) ->
    {Path,_} = lists:split(length(H)-1,H),
    PathStripped = stripShit(string:join(Path, " ")),
    parseGET(T,[{path,fm:fixPath(PathStripped)}]);

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

%% @todo Implement parsePOST
parsePOST([H|T],[])->
    todo.
