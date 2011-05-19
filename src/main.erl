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
        io:format("~nBoundary YEEAAAH: ~p~n", [Boundary]),
	case gen_tcp:recv(Socket, 0, 3000) of
		{ok, Indata} ->
			String = binary_to_list(Indata),
		io:format("~n~p~n~n", [Indata]),
			case string:rstr(String, Boundary) of
				0 ->
					handleMultiPart(Socket, Boundary, File ++ String);
				_ ->
				        Num2 = string:rstr(String,"\r\n-"),
					File ++ string:sub_string(String, 1, Num2)
			end;
		{error, etimedout} ->
			hej;
		{error, etime} ->
			hej;
		{error, Reason} ->
			{error, Reason}
	end.


handler(Socket) ->
    Outdata = case gen_tcp:recv(Socket, 0) of
        {ok, Indata} ->
		    io:format("Request: ~n~p~n",[Indata]),
			Parsed = parser:parse(binary_to_list(Indata)),
			case Parsed of
				{get, _} -> 
					get:handler(Parsed);
				{post, P} -> 
					File = case lists:keysearch(file, 1, P) of
						{value, {file, F}} ->
							F;
						false ->
							error_mod:handler(enoent)
					end,
					Boundary = case lists:keysearch(boundary, 1, P) of
						{value, {boundary, B}} ->
							B;
						false ->
							error_mod:handler(enoent)
					end,
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
					        {error_eval, Bin} ->
										Bin
					end;
				{error, Reason} ->
					error_mod:handler(Reason);
			        {error_eval, Bin} ->
				        Bin
			end;
		{error, closed} ->
			{LoL, Bin} = error_mod:handler(socket),
		        Bin
    end,
    io:format("Answer: ~n~p~n",[Outdata]),
    gen_tcp:send(Socket, Outdata),
    gen_tcp:close(Socket).
	
	
	
