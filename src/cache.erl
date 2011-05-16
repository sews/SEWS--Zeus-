%%% File    : cache.erl
%% @author Group 6
%% @doc A ETS-cache created for implementation in SEWS 
%% @version 0.1
%% created may 16 ,2001




-module(cache).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(ETS_OPTIONS,[set,named_table,public]).


%% start()
%% @spec start() -> etstable
%% @doc creates a new etstable with the name "etstab". ETS_OPTIONS pre defined.

start()->
    ets:new(etstab,?ETS_OPTIONS).

%% read(Path)
%% @spec string() -> binary()
%% @doc Convert the given Path to Binary code. Side_effects: Store, Get or restore the given Path in the ETS table created from start().

read(Path) ->
    IsDir = filelib:is_dir(Path),
    IsFile = filelib:is_file(Path),
    if
	IsDir ->
	    {error,eisdir};
	IsFile ->
	    case file:read_file_info(Path) of
		{error, Reason} ->
		    {error, Reason};
		{ok, FileInfo} ->
		    Date = element(6,FileInfo),
		    case ets:member(etstab,Path) of
			true ->
			    EtsDate = ets:lookup_element(etstab,Path,2),
			    if
				EtsDate == Date ->
				    io:format("Up to date"),
				    ets:lookup_element(etstab,Path,3);
				true ->
				    {ok, Bin} = file:read_file(Path),
				    ets:insert(etstab,{Path,Date,Bin}),
				    io:format("DATE EXPIRED"),			     
				    Bin
			    end;
			_ ->
			    {ok, Bin} = file:read_file(Path),
			    ets:insert(etstab,{Path,Date,Bin}),
			    io:format("Not in"),
			    Bin
		    end
	    end;
	true ->
	    {error,enoent}
    end.
    
			    
		  
%% TEST CASES


  read_test() ->
    start(),
    {ok, Bin} = file:read_file("./cache.erl"),
   [?assertEqual(Bin,read("./cache.erl")),
    ?assertEqual({error,eisdir},read("./")),
    ?assertEqual({error,enoent},read("./gaeha/fade"))].

