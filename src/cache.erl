%% @author Group 6
%% @doc A cache module

-module(cache).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(ETS_OPTIONS,[set,named_table,public]).
-define(MAX_FILE_SIZE,2000000).
-define(MAX_CACHE_SIZE,51).

%% start()
%% @doc creates a new etstable with the name "etstab". ETS_OPTIONS pre defined.

max_cache_size() ->
    ?MAX_CACHE_SIZE.

max_file_size() ->
    ?MAX_FILE_SIZE.

start() ->
    start(etstab ,fun(X,Y) -> cache_kf:lru(X,Y) end,?MAX_CACHE_SIZE,?MAX_FILE_SIZE).

start(Name)->
    start(Name,fun(X,Y) -> cache_kf:lru(X,Y) end,?MAX_CACHE_SIZE,?MAX_FILE_SIZE).

start(Name,Fun,MaxCacheSize,MaxFileSize) -> 
    ets:new(Name,?ETS_OPTIONS),
    ets:insert(Name,{metadata,[{max_cache_size,MaxCacheSize+1}, {killfun,Fun}, {max_file_size,MaxFileSize}, {etslist,[]}]}).
    
%% read()
%% @doc Convert the given Path to Binary code. Side_effects: Store, Get or restore the given Path in the ETS table created from start().
read(Path) ->
    read(Path, etstab).

read(Path, Name) ->
    MetaList = ets:lookup_element(Name,metadata,2),
    {value,{killfun,Fun}} = lists:keysearch(killfun,1,MetaList),
    IsDir = filelib:is_dir(Path),
    IsFile = filelib:is_file(Path),
    IsDyn = 
	TypeIndex = 	
	case  string:rchr(Path,$.) of
	    0 -> 1;
	    Index -> Index
	end,
	case string:sub_string(Path,TypeIndex) of
	    ".dyn" -> true;
	    _ -> false
	end,
    if
	IsDyn -> dynerl:match(binary_to_list(file:read_file(Path)));
	IsDir ->
	    {error,eisdir};
	IsFile ->
	    case file:read_file_info(Path) of
		{error, Reason} ->
		    {error, Reason};
		{ok, FileInfo} ->
		    Date = element(6,FileInfo),
		    Size = element(2,FileInfo),
		    {value,{max_file_size,MaxFileSize}} = lists:keysearch(max_file_size,1,MetaList),
		    if
			Size < MaxFileSize -> 
			    case ets:member(Name,Path) of
				true ->
				    Fun(Path,Name),  %% lru(Path)
				    EtsDate = ets:lookup_element(Name,Path,2),
				    if
					EtsDate == Date ->
					    %%io:format("Were up to date"),
					    ets:lookup_element(Name,Path,3);
					true ->
					    {ok, Bin} = file:read_file(Path),
					    ets:insert(Name,{Path,Date,Bin}),
					    %%io:format("Change since last read, updated"),	   		     
					    Bin
				    end;
				_ ->
				    Fun(Path,Name), %% lru(Path)
				    {ok, Bin} = file:read_file(Path),
				    ets:insert(Name,{Path,Date,Bin}),
				    %%io:format("Were not in table, now inserted"),
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

read(Path,_,nocache)->    
    {ok, Bin} = file:read_file(Path),
    Bin;
read(Path,Name,cache) ->
    read(Path,Name).
%%% TEST CASES


 %% read_test() ->
 %%   start(),
 %%    {ok, Bin} = file:read_file("./cache.erl"),
 %%    [?assertEqual(Bin,read("./cache.erl")),
 %%     ?assertEqual({error,eisdir},read("./")),
 %%     ?assertEqual({error,enoent},read("./gaeha/fade"))].

