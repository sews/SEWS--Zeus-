
-module(fm).
-export([getFile/1, getContents/1, getInfo/2, dirHandler/1, getInfoAll/1, fixPath/1]).

-include_lib("eunit/include/eunit.hrl").


%%	DATATYPES
%%
%%  A filehandle() has the form of {File, Info}. File is a string containing the entire file.
%%  Info contains any additional info about the file. This can be:
%% 
%%  {contenttype, <string>} example: "image/jpeg"
%%  {charset,<string>}.



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

%%		dirHandler(Directory)
%% @spec (Directory::string()) -> ok
%% @doc	Returns returns a sorted list over all files in Directory (with all directories placed before all files).
%%		If Directory does not exist, {error, enoent} is returned. If Directory is a file {error, eisfile} is returned.

dirHandler(Dir) ->
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
									File2Dir, File1Dir, File1Low > File2Low -> false;
									File2Dir == false, File1Dir == false, File1Low > File2Low -> false;
									File2Dir, File1Dir == false -> false;
									true -> true
								end
							end,
							{ok, lists:sort(DirSort, DirListStripped)};
						{error, Reason} ->	%% enoent, eaccess
							error_mod:handler(Reason)
					end;
				false ->	%% not gonna happen
					{error, eisfile}
			end;
		false ->
			%% file not found
			{error, enoent}
			%%error_mod:handler(enoent)
	end.
	
%% @spec (Path::string()) -> ok
%% @doc	Returns a modified version of Path that both begins and ends with the $/. character. If both $/. are already present, Path is returned unchanged.
	
fixPath (Path) ->
	Path1 = case Path of	%% check for "/" at start of path
		[$/ | _] ->	Path;
		_		 ->
			[$/ | Path]
	end,
	case filelib:is_dir(Path1) of
		true ->
			case lists:last(Path1) of	%% check for "/" at end of path
				$/ ->
					Path1;
				_ ->
					Path1 ++ "/"
			end;
		false ->
			Path1
	end.


%% @spec (FileName::string()) -> {FileData, FileInfo} | {error, eisdir} | {error, enoent} | error_bin()
%%
%% @doc Returns 							{ok, FileHandle} 				if file found. 
%%	If FileName does not exist, returns		{error, enoent}
%%	If FileName is a directory, returns 	{error, eisdir} 
%%	For any other error returns 			{error_eval, Bin}
%%
%%  Use FileHandle in all file information functions in this module.
%%

getFile(FileName) -> 
	case file:read_file(FileName) of
		{ok, Bin} ->
			{ok, {binary_to_list(Bin), getFileInfo(FileName)}};
		{error, eisdir} ->
			{error, eisdir};
		{error, enoent} ->
			{error, enoent};
		{error, Reason} ->
			error_mod:handler(Reason)
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
	?assertEqual("text/html", getInfo({[], [{contenttype, "text/html"}]}, contenttype)).
	
getInfoAll_test() ->
	?assertMatch([{contenttype, "text/html"}], getInfoAll({[], [{contenttype, "text/html"}]})).
	
getContents_test() ->
	?assertEqual("hej jag ar en strang ur en fil", getContents({"hej jag ar en strang ur en fil", []})).
	
fixPath_test() ->
	[?assertEqual("/home/", fixPath("home")),
	?assertEqual("/home/", fixPath("/home/")),
	?assertEqual("/home/", fixPath("/home"))].






 
	
	
	
	
