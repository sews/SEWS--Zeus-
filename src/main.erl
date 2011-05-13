-module(main).
-export([start/1,listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%%EXTERNAL FUNCTIONS:
%%start(Port) -> <Pid>
%%Pre: A port that the server will listen on
%%Post: A pid to the listening process
%%S-E: Starts up the Sews server, listening on the given port
start(Port)->
    Tab = cache:start(),
    spawn(main,listen,[Port,Tab]). 

%%INTERNAL FUNCTIONS:
listen(Port,Tab) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket,Tab).

accept(LSocket,Tab) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> handler(Socket,Tab) end),
    accept(LSocket,Tab).

handler(Socket,Tab) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Indata} ->
	    Parsed = sewsparser:parse(binary_to_list(Indata)),
	    Outdata = 
		case Parsed of
		    {get, Parsed_list} -> 
				get:handler(Parsed,Tab);
		    {post, Parsed_list} ->  
				post:handler(Parsed);
		    {error, Reason} ->
				{error_eval, Bin} = error_mod:handler(Reason),
				Bin
		end,
	    io:format("Request: ~n~p~n",[Indata]),
	    io:format("Answer: ~n~p~n",[Outdata]),
	    gen_tcp:send(Socket, Outdata),
	    gen_tcp:close(Socket);
        {error, closed} ->
            ok %% TODO: Fixa errorhantering här
    end.
