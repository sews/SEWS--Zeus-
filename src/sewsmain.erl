-module(sewsmain).
-export([listen/1,listenSpawned/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

% Call sewsmain:listen(Port) to start the service.

listen(Port)->
    spawn(sewsmain,listenSpawned,[Port]). 
		  
listenSpawned(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> server(Socket) end),
    accept(LSocket).


%%Ska nog inte vara en loop, när browsern fått sin fil borde anslutningen stängas
server(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    %% TODO: Fixa en parser som kan ge sökvägen från GET
	    Parsed = sewsparser:parse(binary_to_list(Data)),
	    OutData = case Parsed of
		{get, ParsedList} -> 
	    {ok, Bin} = get:handler(Parsed),
	    %% Det som ska skickas till web browsern.
	    %%TODO?: Automatisera ett svar som ger korrekt data beroende på 
	    %%vilken maskin servern körs på, e.g datum, OS på server-maskinen etc?
%	    Html = list_to_binary([" HTTP/1.1 200 OK
% Date: Mon, 23 May 2005 22:38:34 GMT
% Server: Sews/0.1 (Unix) (Ubuntu/Linux) 
% Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT
% Accept-Ranges: bytes
% Connection: close
% Content-Type: text/html; charset=UTF-8\n\n"]),
	    
	     %% Skriver ut inkommande och utgående trafik i erlang-skalet
	    io:format("~n~p~n",[Data]), %% <- Reqesten som skickades in
	    io:format("~n~p~n",[Html]), %% <- Svaret som skickas tillbaka
	    io:format("~n~p~n",[Bin]),
	    %% Skickar tillbaka och stänger socketen
	    gen_tcp:send(Socket, Html),
	    gen_tcp:send(Socket, Bin),
	    gen_tcp:close(Socket);
        {error, closed} ->
            ok
    end.

