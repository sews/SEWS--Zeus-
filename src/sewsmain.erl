-module(sewsmain).
-export([start/1,listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

% Call sewsmain:listen(Port) to start the service.

start(Port)->
    spawn(sewsmain,listen,[Port]). 
		  
listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> handler(Socket) end),
    accept(LSocket).


%%Ska nog inte vara en loop, när browsern fått sin fil borde anslutningen stängas
handler(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Indata} ->
	    Parsed = sewsparser:parse(binary_to_list(Indata)),
	    {ok, Outdata} = 
		case Parsed of
		    {get, Parsed_list} -> 
			{ok, Bin} = get:handler(Parsed);
		    {post, Parsed_list} ->  
			{ok,Bin} = post:handler(Parsed);
		    {error, Reason} ->
			{ok, Bin} = error_mod:handler(Reason)
		end,
	    %% Skriver ut inkommande och utgående trafik i erlang-skalet
	    io:format("~n~p~n",[Indata]), %% <- Reqesten som skickades in
	    io:format("~n~p~n",[Outdata]), %% <- Svaret som skickas tillbaka
	    %% Skickar tillbaka och stänger socketen
	    gen_tcp:send(Socket, Outdata),
	    gen_tcp:close(Socket);
        {error, closed} ->
            ok %% TODO: Fixa errorhantering h�r
    end.


