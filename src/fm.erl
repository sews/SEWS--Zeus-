
-module(fm).
-export([getFile/1, getContents/1, getInfo/2, dirHandler/1, getInfoAll/1,filesAndDirs/2, fixPath/1]).

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
%% @doc	Returns the path to index.html if such a file exists, otherwise returns a sorted list over all files in Directory (eg all directories are placed before all
%%																																					files)


%%			case file:read_file(Dir ++ "index.html") of
%%				{error, enoent} ->	%% index.html not found
%%				
%%				{ok, Bin} ->
%%					{file, {binary_to_list(Bin), getFileInfo(Dir ++ "index.html")}};
%%					%%{ok, Dir ++ "index.html"};
%%				Kuk ->
%%					Kuk
%%			end.

dirHandler(Dir) ->
	case filelib:is_file(Dir) of
		true ->
			case filelib:is_dir(Dir) of
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
						{error, Reason} ->
							error_mod:handler(Reason)
					end;
				false ->
					{error, eisfile}
			end;
		false ->
			error_mod:handler(enotexist)
	end.
	
fixPath (Path) ->
	case filelib:is_dir(Path) of
		true ->
			case lists:last(Path) of
				$/ ->
					Path;
				_ ->
					Path ++ "/"
			end;
		false ->
			Path
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
			{ok, {binary_to_list(Bin), getFileInfo(FileName)}};
		{error, eisdir} ->
			{error, eisdir};
			%%dirHandler(FileName);
		{error, Reason} ->	%% {error, Reason}
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



%%Pre: A list of filenames and a path to were they are located
%%Post: A list of tuples consisting the tuples were the first element is either the atom isdir if the file
%%      is a directory or isfile if the file is a file and the second element is the filename
%%Ex: filesAndDirs(["text.txt","Documents"],"/home/usr/") => [{isdir,"Documents"},{isfile,"text.txt"}]
%%S-E: None(I think...)
filesAndDirs([],_) -> [];
filesAndDirs([File|FileTail],Path)->
    IsDir = filelib:is_dir(Path ++ File),
    if
	IsDir ->
	    [{isdir,File}|filesAndDirs(FileTail,Path)];
	true -> 
	    [{isfile, File}|filesAndDirs(FileTail,Path)]
    end.


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
