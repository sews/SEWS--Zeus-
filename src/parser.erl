%%%-------------------------------------------------------------------
%%% File    : parser.erl
%%% Author  : Grupp 6
%%% Description : The Sews Parser: Parses HTML requests to a format suitable to erlang.
%%%-------------------------------------------------------------------
-module(parser).
-export([parse/1]).
-compile(export_all). %% For testing purposes

-include_lib("eunit/include/eunit.hrl").

%% @version 0.1
%% @doc The Sews Parser: Parses the requests to a format suitable to erlang.  

-define(SERVER_ROOT,"/var/www").

%% For testing purposes only
string()->
    "GET /home/dennisrosen/big alex on horse.txt HTTP/1.1\r\nHost: 127.0.0.1:8888\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:2.0.1) Gecko/20100101 Firefox/4.0.1\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-us,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 115\r\nConnection: keep-alive\r\n\r\n".



%%%%%%%%%%%%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%
%% @spec parse(Input::String) -> Tuple_list
%% @doc Parser for the html requests.
%%  Pre: A POST or GET request in the standard format
%%  Post: A tuple {type_atom(), TupleList} where TupleList consists of tuples of the format {header_atom(), Data} or a tuple {error, Reason}
%% possible tuples: {path, Path}, {host, Host}, {connection, Connection}
%% S-E: None 
%% Ex: parser:parse("GET /"). -> {get,[{path,"/index.html"}]}

parse(Input)->
    Headers = string:tokens(Input,"\n"),
    [HeadList|MainTail] = lists:map(fun(X) -> string:tokens(X," \r\007") end,Headers),
    [Request|Tail] = HeadList,
    case Request of
        "GET" -> parseGET([Tail|MainTail],[]);
        "POST" -> parsePOST([Tail|MainTail],[]);
        Any -> error_mod:handler(faulty_request)
    end.


%%%%%%%%%%%%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%


%% @doc Returns a modified version of String with every occurence of "%20" substrings replaced with spaces
%% @spec replaceHtmlSpace(String::list) -> String::list

replaceHtmlSpace([]) -> [];
replaceHtmlSpace([E]) -> [E];
replaceHtmlSpace([H1, H2]) -> [H1, H2];
replaceHtmlSpace([H1,H2,H3|Rest]) ->
	case [H1, H2, H3] of
		"%20" ->
			[$ |replaceHtmlSpace(Rest)];
		 _ ->
			[H1|replaceHtmlSpace([H2,H3|Rest])]
	end.
	

%% @spec parseGET(String::list,Tuple::list) -> Tuple::list
%% @todo Implementera för fler headers 
%% @doc Parse out the keywords and the relevant data from the keywords
%% Pre: A correctly formated GET request starting with the path
%% Post: A tupple of the format {get, Tuple_list} or {error, Reason}, where Tuple_list consists of tuples of the format {header_atom(), Data}
%% S-E None

parseGET([[]],ParsedList) ->
    {get, ParsedList};
parseGET([],ParsedList)->
    {get, ParsedList};
parseGET([H|MainTail], []) ->
    {Path,_} = lists:split(length(H)-1,H),
    PathFixed = replaceHtmlSpace(string:join(Path, " ")),
    parseGET(MainTail,[{path,?SERVER_ROOT ++ fm:fixPath(PathFixed)}]);
parseGET([[H|InnerTail]|MainTail],ParsedList) ->
	    case H of
		"Host:" ->
		    [Host|Rest] = InnerTail,
		    parseGET(MainTail,[{host, Host}|ParsedList]);
		"Connection:" ->
		    [Connection_type|Rest] = InnerTail,
		    parseGET(MainTail,[{connection,Connection_type}|ParsedList]);  %% kanske ta med mer info
		"Accept-Language:" ->
		    parseGET(MainTail,[{accept_language,InnerTail}|ParsedList]);
		"User-Agent:" -> 
		    parseGET(MainTail,[{user_agent,InnerTail}|ParsedList]);
		"Accept-Encoding:" ->
		    parseGET(MainTail,[{accept_encoding,InnerTail}|ParsedList]);
		"Accept-Charset:" ->
		    parseGET(MainTail,[{accept_charset,InnerTail}|ParsedList]);
		"Keep-Alive:" ->
		    parseGET(MainTail,[{keep_alive,InnerTail}|ParsedList]);
		Any -> parseGET(MainTail, ParsedList)
    end.


%% Pre: A correctly formated POST request starting with the path
%% Post: A tupple of the format {post, Tuple_list} or {error, Reason}, where Tuple_list consists of tuples of the format {header_atom(), Data}
%% S-E None	    

%% @todo Implement parsePOST
parsePOST([H|T],[])->
    todo.


%% TEST CASES

parse_test() ->
    ?assertEqual([{path,?SERVER_ROOT ++ "/pics/horse.jpeg"}],parse("GET /pics/horse.jpeg")).
