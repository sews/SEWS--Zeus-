-module(cache_kf).
-export([lru/2,killOneRandom/2]).

lru(Path,Name) ->
    MetaList = ets:lookup_element(Name,metadata,2),
    {value,{etslist,LRUList}} = lists:keysearch(etslist,1,MetaList),
    case ets:info(Name) of
	{error, Reason} ->
	    {error,Reason};
	InfoList ->	    
	    {value,{max_cache_size,MaxCacheSize}} = lists:keysearch(max_cache_size,1,MetaList),
	    case lists:keysearch(size,1,InfoList) of
		{value,{size,EtsSize}} ->
		    if 
			EtsSize < MaxCacheSize ->
			    %%io:format("Under Cache SIZE"),
			    case lists:keymember(Path,1,LRUList) of
				false ->				    
				    NewMetaList = lists:keystore(etslist,1,MetaList,{etslist,[{Path}|LRUList]}),
				    ets:insert(Name,{metadata,NewMetaList});
				true -> 
				    NewLRUList = lists:keydelete(Path,1,LRUList),
				    NewMetaList = lists:keystore(etslist,1,MetaList,{etslist,[{Path}|NewLRUList]}),
				    ets:insert(Name,{metadata,NewMetaList})
			    end;
			true ->
			    %%io:format("Over Cache Size"),
			    case lists:keymember(Path,1,LRUList) of
				 false ->
				     {LastPath} = lists:last(LRUList),
				     NewLRUList = lists:keydelete(LastPath,1,LRUList),
				     ets:delete(Name, LastPath),
				     NewMetaList = lists:keystore(etslist,1,MetaList,{etslist,[{Path}|NewLRUList]}),
				     ets:insert(Name,{metadata,NewMetaList});
				true ->
				    NewLRUList = lists:keydelete(Path,1,LRUList),
				    NewMetaList = lists:keystore(etslist,1,MetaList,{etslist,[{Path}|NewLRUList]}),
				    ets:insert(Name,{metadata,NewMetaList})
			    end
		    end;
		false ->
		    error	     
	    end
    end.



killOneRandom(Path,Name) ->
        case ets:info(Name) of
	{error, Reason} ->
	    {error,Reason};
	InfoList ->	    
	    case lists:keysearch(size,1,InfoList) of
		{value,{size,EtsSize}} ->
		    MetaList = ets:lookup_element(Name,metadata,2),
		    {value,{etslist,RandomList}} = lists:keysearch(etslist,1,MetaList),
		    VictimNum = if 
			EtsSize - 2 > 0 ->
			    random:uniform(EtsSize-2);
			true ->
			    false
		    end,
		    {value,{max_cache_size,MaxCacheSize}} = lists:keysearch(max_cache_size,1,MetaList),
		    if 
			EtsSize < MaxCacheSize ->
			    case  lists:keymember(Path, 1, RandomList) of
				false ->
				    NewMetaList = lists:keystore(etslist,1,MetaList,{etslist,[{Path}|RandomList]}),
				    ets:insert(Name,{metadata,NewMetaList});
				true -> 
				    ok
			    end;
			true ->
			    case  lists:keymember(Path, 1, RandomList) of
				false -> 
				    {RandomPath} = lists:nth(VictimNum,RandomList),
				    NewRandomList = lists:keydelete(RandomPath,1,RandomList),
				    ets:delete(Name, RandomPath),
				    NewMetaList = lists:keystore(etslist,1,MetaList,{etslist,[{Path}|NewRandomList]}),
				    ets:insert(Name,{metadata,NewMetaList});
				true ->
				    ok
			    end
		    end;
		false ->
		    error	     
	    end
    end.
