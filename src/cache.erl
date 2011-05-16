-module(cache).
-compile(export_all).

-define(ETS_OPTIONS,[set,named_table,public]).
-definve(MAX_FILE_SIZE,3000000).

start()->
    ets:new(etstab,?ETS_OPTIONS).

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
    
			    
		    
lru()->
    LruList = ets:lookup_element(etstab,lru,2),
    
    
