-module(main).
-export([start/1,listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%%EXTERNAL FUNCTIONS:
%%start(Port) -> <Pid>
%%Pre: A port that the server will listen on
%%Post: A pid to the listening process
%%S-E: Starts up the Sews server, listening on the given port
start(Port)->
    cache:start(),
    spawn(main,listen,[Port]). 

%%INTERNAL FUNCTIONS:
listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> handler(Socket) end),
    accept(LSocket).

handler(Socket) ->
    Outdata = case gen_tcp:recv(Socket, 0) of
        {ok, Indata} ->
		    io:format("Request: ~n~p~n",[Indata]),
			Parsed = parser:parse(binary_to_list(Indata)),
			case Parsed of
				{get, _} -> 
					get:handler(Parsed);
				{post, _} ->  
					post:handler(Parsed);
				{error, Reason} ->
					{error_eval, Bin} = error_mod:handler(Reason),
					Bin
			end;
		{error, closed} ->
		        ok %% TODO: Fixa errorhantering här
    end,
    io:format("Answer: ~n~p~n",[Outdata]),
    case gen_tcp:recv(Socket, 0) of 
    	{ok, Hej} ->
    		io:format(Hej);
    	{error, closed} ->
    		io:format("~n~nNO MORE SOCKET THINGS~n~n"),
    		ok
    end,
    gen_tcp:send(Socket, Outdata),
	gen_tcp:close(Socket).
