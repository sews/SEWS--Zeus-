%% @author Grupp 6
%% @version 0.1
%% @since 12.05.11

-module(main).
-export([start/0, start/1,start/2, listen/1,listen/2]).

-include("../include/config.hrl").

%%//==================\\
%%||EXTERNAL FUNCTIONS:||
%%\\==================//
%% start() -> Pid
%% @spec () -> Pid
%% @doc Starts the SEWS server using the default port

start() ->
    start(?DEFAULT_PORT).

%% start(Port) -> Pid
%% @spec (Port::int) -> Pid
%% @doc Start the SEWS server using Port as port

start(Port)->
    spawn(main,listen,[Port]). 

%% start(Port, FunAtom)
%% @spec (Port::int, FunAtom::string) -> Pid
%% @doc Start the SEWS server using Port as port
%%      and the cache with FunAtom as given algorithm

start(Port,FunAtom)->
    spawn(main,listen,[Port,FunAtom]).

%% listen(Port)
%% @spec (Port::int) -> Pid
%% @doc Listens to the given port Port using pre-defined
%%      TCP_OPTIONS 

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    cache:start(etstab),
    accept(LSocket).

%% list(Port, FunAtom)
%% @spec (Port::int, FunAtom::string) -> Pid
%% @doc Listens to the given port Port using pre-defined
%%      TCP_OPTIONS and starts the cache with FunAtom as
%%      given algorithm

listen(Port,FunAtom) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    case FunAtom of
	kor ->
	    cache:start(etstab,fun cache_kf:killOneRandom/2,50,2000000);
	lru ->
	    cache:start(etstab,fun cache_kf:lru/2,50,2000000)
    end,
    accept(LSocket).

%% //===================\\
%% ||INTERNAL FUNCTIONS:||
%% \\===================//
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> handler(Socket) end),
    accept(LSocket).


handleMultiPart(Socket, Boundary, File) ->
    case gen_tcp:recv(Socket, 0, 30000) of		%% receive socket data and forward it to the parser
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
		   %%io:format("Request: ~n~p~n",[Indata]),
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



