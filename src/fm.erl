
-module(fm).
-export([getFile/1, getContents/1, getInfo/2, dirHandler/1, getInfoAll/1, fixPath/1, uploadFile/2]).

-include_lib("eunit/include/eunit.hrl").

%% WEB ROOT FOR SEWS (usually /var/www)
-define(WWW_ROOT, "/home").

%%	DATATYPES
%%
%%  A filehandle() has the form of {File, Info}. File is a string containing the entire file.
%%  Info contains any additional info about the file. This can be:
%% 
%%  {contenttype, <string>} example: "image/jpeg"
%%  {charset,<string>}.


%% 			INTERNAL FUNCTIONS

%%		dirContentType(FileName)
%% @spec (FileName::string()) -> String
%% @doc	Returns a string containing html content-type for the supplied filename

getContentType(FileName) ->
	case string:sub_word(FileName, 2, $.) of
		"jpg"	->
			"image/jpeg";
		"txt"	->
			"text/plain";
		"html"	->
			"text/html";
		"htm"	->
			"text/html";
		"pdf"	->
			"application/pdf";
		_ ->
			"application/octet-stream"
	end.
	
	
%%		dirCharSet(FileName)
%% @spec (FileName::string()) -> String
%% @doc	Returns a string containing html char-set for the supplied filename
	
getCharset (_) ->
	"UTF-8".
	
	
%%		getFileInfo(FileName)
%% @spec (FileName::string()) -> {InfoType, InfoValue}
%% @doc	Returns a 2-tuple list containing info key-value pairs for the supplied filename

getFileInfo(FileName) ->
	[	{contenttype, getContentType(FileName)},
		{charset, getCharset(FileName)}		].
	
	
%%			EXPORTED FUNCTIONS

%%		dirHandler(Directory)
%% @spec (Directory::string()) -> {ok, DirList} | {error, Reason} | {error_eval, Bin}
%% @doc	Returns a sorted list over all files in Directory (with all directories placed before all files).
%%		If Directory does not exist, {error, enoent} is returned. If Directory is a file {error, eisfile} is returned.


dirHandler(WebDir) ->
	Dir = ?WWW_ROOT ++ WebDir,
	case filelib:is_file(Dir) of	%% if Dir exists
		true ->
			case filelib:is_dir(Dir) of	%% if Dir is a directory
				true ->
					case file:list_dir(Dir) of
						{ok, DirList} ->
							DirListStripped = lists:filter(fun ([A | _]) -> A =/= $. end, DirList),	%% strip all ".*" files (eg hidden files)
							DirSort = fun(File1, File2) ->
								File1Dir = filelib:is_dir(Dir ++ File1),
								File2Dir = filelib:is_dir(Dir ++ File2),
								File1Low = string:to_lower(File1),
								File2Low = string:to_lower(File2),
								if
									File1Dir == false, File2Dir 			-> false;
									File1Dir == false, File1Low > File2Low 	-> false;
									File2Dir, File1Low > File2Low 			-> false;
									true -> true
								end
							end,
							{ok, lists:sort(DirSort, DirListStripped)};
						Error ->
							Error
					end;
				false ->	%% not gonna happen
					{error, eisfile}
			end;
		false ->
			%% file not found
			{error, enoent}
	end.
	
%% @spec (Path::string()) -> FixedPath
%% @doc	Returns a modified version of Path that both begins and ends with the $/. character. If both $/. are already present, Path is returned unchanged.
	
fixPath (Path) ->
	Path2 = case Path of	%% check for "/" at start of path
		[$/ | _] ->	
			Path;
		_		 ->
			[$/ | Path]
	end,
	%%Path2 = case string:str(Path1, ?WWW_ROOT) of	%% check for web root
	%%	1 ->
	%%		Path1;
	%%	Any ->
	%%		?WWW_ROOT ++ Path1
	%%end,
	case filelib:is_dir(?WWW_ROOT ++ Path2) of
		true ->
			case lists:last(Path2) of	%% check for "/" at end of path
				$/ ->
					Path2;
				_ ->
					Path2 ++ "/"
			end;
		false ->
			Path2
	end.


uploadFile (WebPath, FileContents) ->
    FileName = ?WWW_ROOT ++ WebPath,
	case file:write_file(FileName, list_to_binary(FileContents)) of
		ok ->
			ok;
		{error, eisdir} ->
			{error, enoname};
		Error ->
			Error
	end.

%% @spec (FileName::string()) -> {ok,FileData, FileInfo} | {error, eisdir} | {error, enoent} | {error_eval, Bin}
%%
%% @doc Returns 							{ok, FileHandle}	if file found. 
%%	If FileName does not exist, returns		{error, enoent}
%%	If FileName is a directory, returns 	{error, eisdir} 
%%	For any other error returns 			{error_eval, Bin}
%%
%%  Use FileHandle in all file information functions in this module.
%%

getFile(WebPath) ->
	Path = ?WWW_ROOT ++ WebPath,
    IsDir = filelib:is_dir(Path),
    IsFile = filelib:is_file(Path),
    if
		IsDir -> 
		    {error, eisdir};
		IsFile -> 
	    	Bin = cache:read(Path),
	    	{ok,{binary_to_list(Bin),getFileInfo(Path)}};
		true -> 
	    	{error, enoent}
    end.


%% 	 	getContents (FileHandle)
%% @doc 	Returns a string containing the entire file
%% @spec (FileHandle::{FileData, FileInfo}) -> List

getContents({FileList, _}) -> FileList.


%% 		getInfoAll (FileHandle)
%% @doc 	Returns a list containing all {Infokey, Infovalue} pairs from FileHandle
%% @spec (Filehandle::{FileData, FileInfo}) -> List

getInfoAll({_, InfoList}) -> InfoList.


%% 	 	getInfo (FileHandle, Info)
%% @doc	Returns the corresponding file info in filehandle FileHandle to the atom Info, or false if it is not found.
%%			Example:	getInfo("hej.txt", contenttype)
%% @spec (Filehandle::{FileData, FileInfo}, List) -> List
	
getInfo({_, InfoList}, Info) ->
	case lists:keysearch(Info, 1, InfoList) of
		{value, {_, Value}} ->
			Value;
		false ->
			{error, noinfo}
	end.
	
	
%%	Typical error reasons:
%%
%%	enoent
%%  The file does not exist.
%%
%%	eacces
%%  Missing permission for reading the file or searching one of the parent directories.
%%
%%	eisdir
%%  The named file is not a regular file. It may be a directory, a fifo, or a device.
%%
%%	enotdir
%%  A component of the file name is not a directory. On some platforms, enoent is returned instead.
%%
%%	enospc
%%  There is a no space left on the device (if write access was specified).


%% TEST CASES

getInfo_test() ->
	?assertEqual("text/html", getInfo({[], [{contenttype, "text/html"}]}, contenttype)).
	
getInfoAll_test() ->
	?assertMatch([{contenttype, "text/html"}], getInfoAll({[], [{contenttype, "text/html"}]})).
	
getContents_test() ->
	?assertEqual("hej jag ar en strang ur en fil", getContents({"hej jag ar en strang ur en fil", []})).
	
%%fixPath_test() ->
%%	[?assertEqual("/home/", fixPath("home")),
%%	?assertEqual("/home/", fixPath("/home/")),
%%	?assertEqual("/home/", fixPath("/home"))].






 

