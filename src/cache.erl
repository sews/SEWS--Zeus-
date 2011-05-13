-module(cache).
-compile(export_all).

-define(ETS_OPTIONS,[set]).

start()->
    ets:new(name,?ETS_OPTIONS).

read(Tab,Path) ->
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
		    case ets:member(Tab,Path) of
			true ->
			    EtsDate = ets:lookup_element(Tab,Path,2),
			    if
				EtsDate == Date ->
				  %  io:format("Up to date"),
				    ets:lookup_element(Tab,Path,3);
				true ->
				    {ok, Bin} = file:read_file(Path),
				    ets:insert(Tab,{Path,Date,Bin}),
				   % io:format("DATE EXPIRED"),			     
				    Bin
			    end;
			_ ->
			    {ok, Bin} = file:read_file(Path),
			    ets:insert(Tab,{Path,Date,Bin}),
			   % io:format("Not in"),
			    Bin
		    end
	    end;
	true ->
	    {error,enoent}
    end.
    
			    
		    
