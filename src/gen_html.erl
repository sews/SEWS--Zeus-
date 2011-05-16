%% @version 0.2
%% @doc A module made for generating html code
%% @since 12.05.11

-module(gen_html).
%-export([dirDoc/2]).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% //==================\\
%% ||INTERNAL FUNCTIONS||
%% \\==================//

%% dirDocAux(DirList, Path, Html)
%% @hidden@spec (DirList::list, Path::list, Html::list) -> String::list
%% @hidden@doc Generates a simple UI to the file or directory at DirList.
%%             The path to the current folder is contained at all points of execution
%%             in the Path variable. The html code is saved in the variable Html.
%%             Returns a string containing a html document.
%% @hidden@private Removed Host as a variable
%% @since 12.05.11

dirDocAux(DirList, Path, [])->
    dirDocAux(DirList, Path, "<html><head><title>Index of " ++ Path ++ "</title></head><body><h1>Index of " ++ Path ++ "</h1><hr>");
dirDocAux([File|FileTail], Path, Html) ->
    IsDir = filelib:is_dir(Path ++ File),
    case IsDir of
	 true -> %% File refers to a directory
	    dirDocAux(FileTail, Path, Html ++ "[Dir] <a href='" ++ Path ++ File ++ "'>" ++ File ++ "</a>" ++ "<br />");
	false -> %% File refers to a file
	    dirDocAux(FileTail, Path, Html ++ "[File] <a href='" ++ Path ++ File ++ "'>" ++ File ++ "</a>" ++ "<br />")
    end;
dirDocAux([], _, Html) ->
    Html ++ "<hr></body></html>".
    
%% //==================\\
%% ||EXPORTED FUNCTIONS||
%% \\==================//

%% @doc Returns a string containing HTML code to be used in a webserver,
%%      generated from files in DirList and headers in HList
%% @spec dirDoc(DirList::list, Hlist::list) -> String::list
%% @since 12.05.11

dirDoc(DirList, HList)-> 

%% Get path so requested file from HList
    Path = case lists:keysearch(path, 1, HList) of 
    	{value,{path, P}} ->
		   	P;
	       	false ->
		   	error_mod:handler(nopath)
	   	end,
    dirDocAux(DirList, Path, []).
    
    
%% TEST CASES

dirDoc_test() ->
	?assertEqual("<html><head><title>Index of hej</title></head><body><h1>Index of hej</h1><hr><hr></body></html>", dirDoc([], [{path, "hej"},{host, ""}])).


%% serverHeaders() ->
%%     "HTTP/1.1 200 OK:
%% Content-Type: "++ contType() ++ "
%% Last-Modified: " ++ lastMod()  ++  "
%% Accept-Ranges: bytes
%% Server: Sews Server version 0.2
%% Date: "++ dateHeader()  ++"
%% Connection: keep-alive
%% Content-Length:" ++ contLength().

dateHeader() ->
    {{Year,Month,Day},{Hour,Min,Seconds}} = calendar:universal_time(),
    weekday({Year,Month,Day}) ++ ", " ++ atoi(Day)++ " " ++ month(Month) ++ " " ++ atoi(Year) ++ " " ++ hours({Hour,Min,Seconds}) ++ " GMT".

weekday(Date) ->
    Num = calendar:day_of_the_week(Date),
    case Num of
	1 -> "Mon";
	2 -> "Tue";
	3 -> "Wen";
	4 -> "Thu";
	5 -> "Fri";
	6 -> "Sat";
	7 -> "Sun";
	_ -> error
    end.

month(Month) ->
    case Month of
	1 -> "Jan";
	2 -> "Feb";
	3 -> "Mar";
	4 -> "Apr";
	5 -> "May";
	6 -> "Jun";
	7 -> "Jul";
	8 -> "Aug";
	9 -> "Sep";
	10 -> "Oct";
	11 -> "Nov";
	12 -> "Dec";
	_  -> error
    end.
	    
hours({Hour,Min,Seconds}) ->
    FormHours = if
		    Hour < 10 -> "0" ++ atoi(Hour);
		    true -> atoi(Hour)
    end,
    FormMin = if
		  Min < 10 -> "0" ++ atoi(Min);
		  true -> atoi(Min)
	      end,
    FormSec = if
		  Seconds < 10 -> "0" ++ atoi(Seconds);
		  true -> atoi(Seconds)
	      end,
    FormHours ++ ":" ++ FormMin ++ ":" ++ FormSec.
			  
    
	    
    


%%% Converts an integer to a string	    
atoi(Num) ->
    lists:flatten(io_lib:format("~p", [Num])).
