-module(main).
-export([start/0, start/1, listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(DEFAULT_PORT, 8080).

%%EXTERNAL FUNCTIONS:
%%start(Port) -> <Pid>
%%Pre: A port that the server will listen on
%%Post: A pid to the listening process
%%S-E: Starts up the Sews server, listening on the given port
start() ->
    start(?DEFAULT_PORT).

start(Port)->
    cache:start(etstab),  %% FIX NAME ,START FROM COMMAND.
    spawn(main,listen,[Port]). 



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
    Part = case lists:keysearch(part, 1, P) of 
	       {value, {part, Pa}} ->
		   Pa;
	       false ->
		   {error, enoent}
	   end,
    File = case lists:keysearch(file, 1, P) of 
	       {value, {file, F}} ->
		   F;
	       false ->
		   {error, enoent}
	   end,
    Boundary = case lists:keysearch(boundary, 1, P) of 
		   {value, {boundary, B}} ->
		       B;
		   false ->
		       {error, enoent}
	       end,
    if 	
	Part 		== {error, enoent};	%% handle errors
	File 		== {error, enoent};
	Boundary 	== {error, enoent} ->
	    {error, enoent};
	true ->
	    case Part of
		multipart ->
		    case handleMultiPart(Socket, Boundary, File) of
			{error, Reason} ->
			    error_mod:handler(Reason);
			MegaFile ->
			    MegaParsed = lists:keyreplace(file, 1, P, {file, MegaFile}),
			    post:handler({post, MegaParsed})
		    end;
		single ->
		    post:handler(Parsed);
		E ->	%% not gonna happen
		    E
	    end
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
			   {error, Reason}
		   end;
	       E ->
		   E
	   end,
    Outdata = case Data of 
		  {error_eval, B} ->	%% should not happen anymore
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



