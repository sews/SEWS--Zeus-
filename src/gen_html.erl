%% @doc A module made for generating html code

-module(gen_html).
%%%-export([dirDoc/2]).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(WWW_ROOT, "/home").

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

dirDocAux(DirList, Path, Mode, []) ->
    HTMLString = gen200Headers() ++ "<html><head><title>Index of " ++ Path ++ "</title></head><body>

	<table><tr><td><h1>Index of " ++ Path ++ "</h1><hr>",

	case Mode of
	    dirlist ->
    		dirDocAux(DirList, Path, Mode, HTMLString);
	    upload ->
    		dirDocAux(DirList, Path, Mode, HTMLString ++ "<h2>File successfully uploaded :D</h2><hr>")
	end;
dirDocAux([File|FileTail], Path, Mode, Html) ->
    IsDir = filelib:is_dir(?WWW_ROOT ++ Path ++ File),
    case IsDir of
	true -> %% File refers to a directory
	    dirDocAux(FileTail, Path, Mode, Html ++ "[Dir] <a href='" ++ Path ++ File ++ "'>" ++ File ++ "</a>" ++ "<br />");
	false -> %% File refers to a file
	    dirDocAux(FileTail, Path, Mode, Html ++ "[File] <a href='" ++ Path ++ File ++ "'>" ++ File ++ "</a>" ++ "<br />")
    end;
dirDocAux([], _, _, Html) ->
    Html ++ "</td><td>

    	<FORM action=\"\" 
    		  enctype=\"multipart/form-data\"
			  method=\"post\">
			<P>
	<INPUT 	type=\"file\"  
          				name=\"fileselect\"
          				value=\"defaultfile\">
        	<P>

	<INPUT 	type=\"submit\" 
        				value=\"Upload\">
		</FORM>


	</td></tr></table><hr></body></html>".

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
				  dirDocAux(DirList, Path, dirlist, []).


postHTML (Dir, _, Path) -> dirDocAux (Dir, Path, upload, []).

gen200Headers() ->
    "HTTP/1.1 200 OK:\r\n" ++ "Content-Type: text/html" ++  "\r\nAccept-Ranges: bytes\r\nServer: Sews Server version 0.2\r\nDate: "++ dateFormatted(calendar:universal_time()) ++"\r\nConnection: close\r\n\n".

server200Headers(Path) ->
    FileInfo = 
	case file:read_file_info("/home" ++Path) of
	    {ok, Fileinfo} -> Fileinfo;
	    _ -> nofile
	end,
    {LastModTime, Size} = 
	case FileInfo of
	    nofile -> {nofile,nofile};
	    _ ->
		{element(6,FileInfo),element(2,FileInfo)}
	end,
    "HTTP/1.1 200 OK:\r\n" ++ contentType(Path) ++ lastModified(LastModTime)  ++  "\r\nAccept-Ranges: bytes\r\nServer: Sews Server version 0.2\r\nDate: "++ dateFormatted(calendar:universal_time())++ contentLength(Size) ++"\r\nConnection: close\r\n\n".


contentType(Path)->
    case fm:getContentType(Path) of
	"" -> "Content-Type: text/html";
	Extension -> "Content-Type: " ++ Extension
    end.
	
contentLength(nofile) -> "";
contentLength(Size) ->
    "\r\nContent-Length:" ++ atoi(Size).
    
    
lastModified(nofile)-> "";
lastModified(Time) ->
    "\r\nLast-Modified: " ++ dateFormatted(Time).
    

dateFormatted(Time) ->
    {{Year,Month,Day},{Hour,Min,Seconds}} = Time,
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


%% TEST CASES

%%dirDoc_test() ->
%%	?assertEqual("<html><head><title>Index of hej</title></head><body><h1>Index of hej</h1><hr><hr></body></html>", dirDoc([], [{path, "hej"},{host, ""}])).



