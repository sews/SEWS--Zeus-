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
	case gen_tcp:recv(Socket, 0, 3000) of		%% receive socket data and forward it to the parser
		{ok, Indata} ->
			case parser:parseMultiPart(binary_to_list(Indata), Boundary, File) of
				{continue, Part} ->
					handleMultiPart(Socket, Boundary, Part);
				{done, Part} ->
					Part
			end;
		E ->
			E
	end.
	
	
prepOSTProcessing (Parsed, Socket) ->
	{post, P} = Parsed,
	case lists:keysearch(file, 1, P) of
		{value, {file, File}} ->
			case lists:keysearch(boundary, 1, P) of
				{value, {boundary, Boundary}} ->
					case lists:keysearch(part, 1, P) of
						{value, {part, multipart}} ->
							case handleMultiPart(Socket, Boundary, File) of
								{error, Reason} ->
									error_mod:handler(Reason);
								MegaFile ->
									MegaParsed = lists:keyreplace(file, 1, P, {file, MegaFile}),
									post:handler({post, MegaParsed})
							end;
						{value, {part, single}} ->
							post:handler(Parsed);
						false ->
							error_mod:handler(enoent)
					end;
				false ->
					error_mod:handler(enoent)
			end;
		false ->
			error_mod:handler(enoent)
	end.


handler(Socket) ->
    Data = case gen_tcp:recv(Socket, 0) of
        {ok, Indata} ->
		    io:format("Request: ~n~p~n",[Indata]),
			Parsed = parser:parse(binary_to_list(Indata)),
			case Parsed of
				{get, _} -> 
					get:handler(Parsed);
				{post, _} -> 
					prepOSTProcessing(Parsed, Socket);
				{error, Reason} ->
					error_mod:handler(Reason);
			    {error_eval, Bin} ->
				    Bin
			end;
		E ->
			E
    end,
    Outdata = case Data of 
    	{error_eval, B} ->
    		B;
    	{error, R} ->
    		{error_eval, B} = error_mod:handler(R),
    		B;
    	Any ->
    		Any
    end,
    %%io:format("Answer: ~n~p~n",[Outdata]),
    gen_tcp:send(Socket, Outdata),
    gen_tcp:close(Socket).
	
	
	
