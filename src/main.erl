-module(main).
-export([start/0,listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-define(DEFAULT_PORT, 8080).

%%EXTERNAL FUNCTIONS:
%%start(Port) -> <Pid>
%%Pre: A port that the server will listen on
%%Post: A pid to the listening process
%%S-E: Starts up the Sews server, listening on the given port
start()->
    cache:start(),
    spawn(main,listen,[?DEFAULT_PORT]). 

%%INTERNAL FUNCTIONS:
listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> handler(Socket) end),
    accept(LSocket).
    
handleMultiPart(Socket, Boundary, File) ->
	case gen_tcp:recv(Socket, 0, 1337) of
		{ok, Indata} ->
			String = binary_to_list(Indata),
			case string:rstr(String, "\r\n" ++ Boundary) of
				0 ->
					handleMultiPart(Socket, Boundary, File ++ String);
				Num ->
					Substr = string:sub_string(String, 1, length(String) - Num),
					handleMultiPart(Socket, Boundary, File ++ Substr)
			end
		{error,  ->
			eror
	end,
	timeout.


handler(Socket) ->
    Outdata = case gen_tcp:recv(Socket, 0) of
        {ok, Indata} ->
		    io:format("Request: ~n~p~n",[Indata]),
			Parsed = parser:parse(binary_to_list(Indata)),
			case Parsed of
				{get, _} -> 
					get:handler(Parsed);
				{post, P} -> 
					MegaParsed = case lists:keysearch(part, 1, P) of
						{value, {part, continue}} ->
							handleMultiPart(Socket, P, Boundary);
						false ->
							fack
					end, 
					case post:handler(MegaParsed) of

				{error, Reason} ->
					{error_eval, Bin} = error_mod:handler(Reason),
					Bin
			end;
		{error, closed} ->
		        ok %% TODO: Fixa errorhantering här
    end,
    io:format("Answer: ~n~p~n",[Outdata]),
    gen_tcp:send(Socket, Outdata),
	gen_tcp:close(Socket).
	
	
	
