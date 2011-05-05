-module(sewsnet).
-export([listen/1,listenSpawned/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

% Call echo:listen(Port) to start the service.

%% TODO: Går inte att starta upp med port 80, måste fixas. Om man försöker:
%%=ERROR REPORT==== 4-May-2011::11:32:21 ===
%%Error in process <0.102.0> with exit value: {{badmatch,{error,eacces}},[{sewsnet,listenSpawned,1}]}

listen(Port)->
    spawn(sewsnet,listenSpawned,[Port]). 
		  
listenSpawned(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> loop(Socket) end),
    accept(LSocket).


%%Ska nog inte vara en loop, när browsern fått sin fil borde anslutningen stängas.
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            %gen_tcp:send(Socket, Data),
	    %% TODO: Fixa en parser som kan ge sökvägen från GET
	    Path = "/home/johannes/page.html", 
	    {ok,Bin} = file:read_file(Path),
	    %% Det som ska skickas till web servern.
	    %%TODO?: Automatisera ett svar som ger korrekt data beroende på 
	    %%vilken maskin servern körs på, e.g datum, OS på server-maskinen etc?
	    Html = list_to_binary([" HTTP/1.1 200 OK
 Date: Mon, 23 May 2005 22:38:34 GMT
 Server: Sews/0.1 (Unix) (Ubuntu/Linux) 
 Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT
 Accept-Ranges: bytes
 Connection: close
 Content-Type: text/html; charset=UTF-8\n\n",Bin]),
	    
	    io:format("~n~p~n",[Data]), %% Skriver ut inkommande och utgående trafik i erlang-skalet
	    io:format("~n~p~n",[Html]),
	    gen_tcp:send(Socket, Html),
	    gen_tcp:close(Socket);
            %loop(Socket);
        {error, closed} ->
            ok
    end.


