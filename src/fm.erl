
-module(fm).
-export([getFile/1, getContents/1, getInfo/2, dirHandler/1, getInfoAll/1]).

-include_lib("eunit/include/eunit.hrl").

%% 			INTERNAL FUNCTIONS

getContentType(FileName) ->
	case string:sub_word(FileName, 2, $.) of
		"jpg" ->
			"image/jpeg";
		"txt" ->
			"text/plain";
		"html" ->
			"text/html";
		_ ->
			"text/plain"
	end.
	
getCharset (FileName) ->
	"UTF-8".

getFileInfo(FileName) ->
	[	{contenttype, getContentType(FileName)},
		{charset, getCharset(FileName)}		].
	
	
%%			EXPORTED FUNCTIONS

%%		listDir(Directory)
%% @spec (Directory::string()) -> ok
%% @doc	Returns the path to index.html if such a file exists, otherwise returns a list over all files in Directory.

dirHandler(Dir) ->
			case file:read_file(Dir ++ "index.html") of
				{error, enoent} ->	%% index.html not found
					{dirlist, file:list_dir(Dir)};
				{ok, Bin} ->
					{file, {binary_to_list(Bin), getFileInfo(Dir ++ "index.html")}};
					%%{ok, Dir ++ "index.html"};
				Kuk ->
					Kuk
			end.


%% @spec (FileName::string()) -> {FileData, FileInfo}
%%
%% @doc Returns 							{file, FileHandle} 				if file found. 
%%	If FileName is a directory, returns 	{file, FileHandle}			 	if index.html is found. 
%%	If index.html is not found, returns 	{dirlist, list of all files in directory FileName}
%%  Otherwise returns 						{error, Reason}
%%
%%  Use FileHandle in all other file functions in this module.
%%
%%  Notes: A filehandle has the form of {File, Info}. File is a string containing the entire file.
%%  Info contains any additional info about the file. This can be:
%% 
%%  {contenttype, <string>} example: "image/jpeg"
%%  {charset,<string>}.

getFile(FileName) -> 
	case file:read_file(FileName) of
		{ok, Bin} ->
			{file, {binary_to_list(Bin), getFileInfo(FileName)}};
		{error, eisdir} ->
			dirHandler(FileName);
		ErrorTuple ->	%% {error, Reason}
			ErrorTuple
	end.


%% 	 	getContents (FileHandle)
%%  @doc 	Returns a string containing the entire file	
%% @spec (FileHandle::{FileData, FileInfo}) -> List

getContents({FileList, _}) -> FileList.


%% 		getInfoAll (FileHandle)
%%	@doc 	Returns a list containing all {Infokey, Infovalue} pairs from FileHandle
%% @spec (Filehandle()::{FileData, FileInfo}) -> List

getInfoAll({_, InfoList}) -> InfoList.


%% 	 	getInfo (FileHandle, Info)
%% 	@doc	Returns the corresponding file info in filehandle FileHandle to the atom Info, or false if it is not found.
%%			Example:	getInfo("hej.txt", contenttype)
%% @spec (Filehandle()::{FileData, FileInfo}, List -> List
	
getInfo({_, InfoList}, Info) ->
	case lists:keysearch(Info, 1, InfoList) of
		{value, {_, Value}} ->
			Value;
		false ->
			false
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
	?_assertMatch("text/html", getInfo({[], [{contenttype, "text/html"}]}, contenttype)).
	
getInfoAll_test() ->
	?_assertMatch([{contenttype, "text/html"}], getInfoAll({[], [{contenttype, "text/html"}]})).
	
getContents_test() ->
	?_assertMatch("hej jag ar en strang ur en fil", getContents({"hej jag ar en strang ur en fil", []})).
	


















		
	
		

