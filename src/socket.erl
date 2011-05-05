-module(socket).
-export([listen/1,listenSpawned/1]).

%% @copyright © 2011 SEWS
%% @version 0.1
%% @title SEWS
%% @doc 

-define(DEFAULT_PORT, 80).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

% Call echo:listen(Port) to start the service.

%% @doc Skapar en process som lyssnar på Port
listen(Port)->
    spawn(socket,listenSpawned,[Port]). 

%% @doc Lyssnar på port Port, med egenskaper från macro TCP_OPTIONS		  
listenSpawned(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

%% @doc skapar en ny workerprocess för varje request
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> worker(Socket) end),
    accept(LSocket).

%% @doc Workerprocess som hanterar en request och skickar tillbaka resultatet till klienten
worker(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            %gen_tcp:send(Socket, Data),
	    %% TODO: Fixa en parser som kan ge sÃ¶kvÃ¤gen frÃ¥n GET
	    Path = "/home/johannes/page.html", 
	    {ok,Bin} = file:read_file(Path),
	    %% Det som ska skickas till web servern.
	    %%TODO?: Automatisera ett svar som ger korrekt data beroende pÃ¥ 
	    %%vilken maskin servern kÃ¶rs pÃ¥, e.g datum, OS pÃ¥ server-maskinen etc?
	    Html = list_to_binary([" HTTP/1.1 200 OK
 Date: Mon, 23 May 2005 22:38:34 GMT
 Server: Sews/0.1 (Unix) (Ubuntu/Linux) 
 Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT
 Accept-Ranges: bytes
 Connection: close
 Content-Type: text/html; charset=UTF-8\n\n",Bin]),
	    
	    io:format("~n~p~n",[Data]), %% Skriver ut inkommande och utgÃ¥ende trafik i erlang-skalet
	    io:format("~n~p~n",[Html]),
	    gen_tcp:send(Socket, Html),
	    gen_tcp:close(Socket);
            %loop(Socket);
        {error, closed} ->
            ok
    end.


main([Arg]) ->
    listen(?DEFAULT_PORT).

