%%-------------------------------------------------------------------
%% File    : parser.erl
%% Author  : Grupp 6
%% Description : The Sews Parser: Parses HTML requests to a format suitable to erlang.
%%-------------------------------------------------------------------
-module(parser).
-export([parse/1]).
-compile(export_all). %% For testing purposes

-include_lib("eunit/include/eunit.hrl").

%% @author Grupp 6
%% @doc The Sews Parser: Parses the requests to a format suitable to erlang.  
%% @version 0.1
%% @since 19.05.11

%% For testing purposes only
string()->
    "GET /home/dennisrosen/big%20alex%20on%20horse.txt HTTP/1.1\r\nHost: 127.0.0.1:8888\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:2.0.1) Gecko/20100101 Firefox/4.0.1\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-us,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 115\r\nConnection: keep-alive\r\n\r\n".
    
string2()->
    "GET /home/johe7425 HTTP/1.1\r\nHost: localhost:8888\r\nUser-Agent: Mozilla/5.0 (X11; U; SunOS i86pc; en-US; rv:1.8.1.20) Gecko/20090122 Firefox/2.0.0.20\r\nAccept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\r\nAccept-Language: en-us,en;q=0.5\r\nAccept-Encoding: gzip,deflate\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 300\r\nConnection: keep-alive\r\nReferer: http://localhost:8888/home\r\n\r\n".
    
stringPOST()->
    "POST /home/dennisrosen/POP HTTP/1.1\r\nHost: localhost:8888\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:2.0.1) Gecko/20100101 Firefox/4.0.1\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-us,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 115\r\nConnection: keep-alive\r\nReferer: http://localhost:8877/home/dennisrosen/POP\r\nContent-Type: multipart/form-data; boundary=---------------------------84239866522249001744292470\r\nContent-Length: 230\r\n\r\n-----------------------------84239866522249001744292470\r\nContent-Disposition: form-data; name=\"fileselect\"; filename=\"text.txt\"\r\nContent-Type: text/plain\r\n\r\nhej hej\nlol\n\r\n\r-----------------------------84239866522249001744292470--\r\n".

%% //==================\\
%% ||EXPORTED FUNCTIONS||
%% \\==================//	

%% parse(Input::String)	
%% @spec (Input::String) -> Tuple_list
%% @doc Parser for the html requests.
%%  Pre: A POST or GET request in the standard format
%%  Post: A tuple {type_atom(), TupleList} where TupleList consists of tuples of the format {header_atom(), Data} or a tuple {error, Reason}
%% possible tuples: {path, Path}, {host, Host}, {connection, Connection}
%% S-E: None 
%% Ex: parser:parse("GET /"). -> {get,[{path,"/index.html"}]}
%% @since 19.05.11
    
parse(Input)->
    %%L = token(Input, $\n, [], []),
    Request = string:sub_word(Input, 1),
    case Request of
        "GET" -> 	L = string:tokens(Input, "\n"),
        			[HeadList|MainTail] = lists:map(fun(X) -> string:tokens(X," \r\0") end, L),
        			[_|Tail] = HeadList,
        			parseGET([Tail | MainTail], []);
        "POST" -> 	L = token(Input, $\n, [], []),
        			parsePOST(L, []);
        _ -> error_mod:handler(faulty_request)
    end.

%% //==================\\
%% ||INTERNAL FUNCTIONS||
%% \\==================//

%% token(String::list, Token::char, LastWord::list, Acc::list)
%% @spec (String::list, Token::char, LastWord::list, Acc::list) -> String::list
%% @doc Splits up String into several substrings each separated by Token (better than string:tokens by approx. 17x)
%% Example: token("alex\non\nbig\n\nhorse", $\n, [], []) = ["alex", "on", "big", "", "horse"] 
%% @since 19.05.11

token([], _, LastWord, Acc) -> lists:reverse ([LastWord | Acc]);
token([H | Rest], Token, LastWord, Acc) ->
	case H =:= Token of
		true ->
			token(Rest, Token, [], [LastWord | Acc]);
		false ->
			token(Rest, Token, LastWord ++ [H], Acc)
 	end.

%% replaceHtmlSpace(String::list)
%% @spec (String::list) -> String::list
%% @doc Returns a modified version of String with every occurence of "%20" substrings replaced with spaces
%% @since 19.05.11

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
	
%% parseGET(String::list,Tuple::list)
%% @spec (String::list,Tuple::list) -> Tuple::list
%% @todo Implementera för fler headers 
%% @doc Parse out the keywords and the relevant data from the keywords
%% Pre: A correctly formated GET request starting with the path
%% Post: A tupple of the format {get, Tuple_list} or {error, Reason}, where Tuple_list consists of tuples of the format {header_atom(), Data}
%% S-E None
%% @since 19.05.11

parseGET([[]],ParsedList) ->
    {get, ParsedList};
parseGET([],ParsedList)->
    {get, ParsedList};
parseGET([H|MainTail], []) ->
    [Path|_] = H,
    PathFixed = replaceHtmlSpace(Path),
    parseGET(MainTail,[{path,fm:fixPath(PathFixed)}]);
parseGET([[H|InnerTail]|MainTail],ParsedList) ->
	    case H of
		"Host:" ->
		    [Host|_] = InnerTail,
		    parseGET(MainTail,[{host, Host}|ParsedList]);
		"Connection:" ->
		    [Connection_type|_] = InnerTail,
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
		_ -> parseGET(MainTail, ParsedList)
    end.

%% parseFile(TheList::list)
%% @spec TheList::list -> TheList::list
%% @doc Returns a copy of list with the last two elements deleted
%% @since 19.05.11

parseFile ([_, _]) -> [];
parseFile ([H | Rest]) ->
	[H | parseFile(Rest)].

%% pOSTProcessing(StringList::list)		
%% @spec (StringList::list) -> StringList::list
%% @doc Removes all tuples with a "/r" key from StringList except for key = file
%% @since 19.05.11

pOSTProcessing([]) -> [];
pOSTProcessing([{Atom, Value} | Rest]) ->
	case Atom of
		file ->
			[{Atom, Value} | pOSTProcessing(Rest)];
		_ ->
			[{Atom, lists:delete($\r, Value)} | pOSTProcessing(Rest)]
	end.

%% parsePOSTAux(String::list, Tuple::list)
%% @spec (String::list, Tuple:list) -> (Tuple::list)
%% @doc Parse out the keywords and the relevant data from the keywords
%% @since 19.05.11

parsePOSTAux([], _) -> [];	%% not gonna happen
parsePOSTAux([H | Rest], ParsedList) ->
	Key   = string:sub_word(H, 1),
	Value = case string:chr(H, $\ ) of
		0 ->
			"";
		Any ->
			string:sub_string(H, Any+1)
	end,
	case Key of
		"Content-Disposition:" ->
			Start 			= string:str(Value, "filename=") + 9,
		        StrippedString 	        = lists:delete($\", lists:delete($\", string:sub_string(Value, Start))),
			FileName 		= string:sub_word(StrippedString, 1, $;),
			parsePOSTAux(Rest, [{filename, FileName} | ParsedList]);
		"Content-Type:" ->
			parsePOSTAux(Rest, ParsedList);
		"\r" ->
			[{file, string:join(parseFile(Rest), "\n")} | ParsedList]
	end.

%% parsePOST(String::list,Tuple::list)		
%% @spec (String::list,Tuple::list) -> Tuple::list
%% @todo Implementera för fler headers 
%% @doc Parse out the keywords and the relevant data from the keywords
%% Pre: A correctly formated POST request starting with the path
%% Post: A tuple of the format {post, TupleList} or {error, Reason}, where TupleList consists of tuples of the format {header_atom(), Data}
%% @since 19.05.11

parsePOST([], ParsedList) -> {post, ParsedList};
parsePOST([H | Rest], []) ->
	Path = string:sub_word(H, 2),
	parsePOST (Rest, [{path, Path}]);
parsePOST([H | Rest], ParsedList) ->
	Key 		= string:sub_word(H, 1),
	Value = case string:chr(H, $\ ) of
		0 ->
			"";
		Any ->
			string:sub_string(H, Any+1)
	end,
	case Key of
		"Host:" ->
			parsePOST(Rest,[{host, Value} | ParsedList]);
		"Connection:" ->
			parsePOST(Rest,[{connection, Value} | ParsedList]); %% kanske ta med mer info
		"Accept-Language:" ->
			parsePOST(Rest,[{accept_language, Value} | ParsedList]);
		"User-Agent:" ->
			parsePOST(Rest,[{user_agent, Value} | ParsedList]);
		"Accept-Encoding:" ->
			parsePOST(Rest,[{accept_encoding, Value} | ParsedList]);
		"Accept-Charset:" ->
			parsePOST(Rest,[{accept_charset, Value} | ParsedList]);
		"Keep-Alive:" ->
			parsePOST(Rest,[{keep_alive, Value} | ParsedList]);
		"Referer:" ->
			parsePOST(Rest,[{referer, Value} | ParsedList]);
		"Content-Type:" ->
			MultiPart = string:sub_word(Value, 1, $;),
			Boundary = string:sub_word(H, 2, $=),
			parsePOST(Rest, [{content_type, MultiPart}, {boundary, "--" ++ Boundary} | ParsedList]);
		"Content-Length:" ->
			parsePOST(Rest,[{content_length, Value} | ParsedList]);
		_ ->
			case lists:keysearch(boundary, 1, ParsedList) of
				false ->
					parsePOST(Rest, ParsedList);
				{value, {_, Boundary}} ->
					case Key =:= Boundary of
						true ->
							{post, pOSTProcessing(parsePOSTAux(Rest, []) ++ ParsedList)};
						false ->
							parsePOST(Rest, ParsedList)
					end
			end
	end.

%% //==========\\
%% ||TEST CASES||
%% \\==========//

parse_test() ->
    ?assertEqual({get,[{path,"/pics/horse.jpeg"}]},parse("GET /pics/horse.jpeg")).

replaceHtml_test() ->
    ?assertEqual("en string med mellanrum", replaceHtmlSpace("en%20string%20med%20mellanrum")),
    ?assertEqual("  a s d   ", replaceHtmlSpace("%20%20a%20s%20d%20%20%20")).

parseFile_test() ->
    ?assertEqual([a,b], parseFile([a,b,c,d])).

token_test() ->
    ?assertEqual(["alex", "on", "big", "", "horse"], token("alex\non\nbig\n\nhorse", $\n, [], [])). 
