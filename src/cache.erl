%%% File    : cache.erl
%% @author Group 6
%% @doc A ETS-cache created for implementation in SEWS 
%% @version 0.1
%% created may 16 ,2001




-module(cache).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(ETS_OPTIONS,[set,named_table,public]).
-define(MAX_FILE_SIZE,2000000).
-define(MAX_CACHE_SIZE,51).

%% start()
%% @spec start() -> etstable
%% @doc creates a new etstable with the name "etstab". ETS_OPTIONS pre defined.

start()->
    ets:new(etstab,?ETS_OPTIONS),
    ets:insert(etstab,{etslist,[]}).  %% Help for LRU Implementation Counting

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
		    Size = element(2,FileInfo),
		    if
			Size < ?MAX_FILE_SIZE -> 
			    case ets:member(etstab,Path) of
				true ->
				    lru(Path),
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
				    lru(Path),
				    {ok, Bin} = file:read_file(Path),
				    ets:insert(etstab,{Path,Date,Bin}),
				    io:format("Not in"),
				    Bin
			    end;
			true -> 
			    {ok, Bin} = file:read_file(Path),
			    Bin
		    end
	    end;
	true ->
	    {error,enoent}
    end.
%% lru(Path)
%%@spec string() -> ok | {error,Reason}
%%@doc Handle the implementation of LRU-alogrithm, by add and delete Paths in a LRU-list under the name etslist in etstable.


lru(Path) ->
    EtsLRU = ets:lookup_element(etstab,etslist,2),
    case ets:info(etstab) of
	{error, Reason} ->
	    {error,Reason};
	InfoList ->
	    case lists:keysearch(size,1,InfoList) of
		{value,{size,EtsSize}} ->
		    if 
			EtsSize < ?MAX_CACHE_SIZE ->
			    case lists:keydelete(Path,1,EtsLRU) of
				false ->
				    ets:insert(etstab,{etslist,[{Path}|EtsLRU]});
				NewLRUList -> 
				    ets:insert(etstab,{etslist,[{Path}|NewLRUList]})
			    end;
			true ->
			    case lists:keydelete(Path,1,EtsLRU) of
				false -> 
				    {LastPath} = lists:last(EtsLRU),
				    NewLRUList = lists:keydelete(LastPath,1,EtsLRU),
				    ets:delete(etstab, LastPath),
				    ets:insert(etstab,{etslist,[{Path}|NewLRUList]});
				NewLRUList ->
				    ets:insert(etstab,{etslist,[{Path}|NewLRUList]})
			    end
		    end;
		false ->
		    error	     
	    end
    end.








%% TEST CASES


read_test() ->
    start(),
    {ok, Bin} = file:read_file("./cache.erl"),
    [?assertEqual(Bin,read("./cache.erl")),
     ?assertEqual({error,eisdir},read("./")),
     ?assertEqual({error,enoent},read("./gaeha/fade"))].

