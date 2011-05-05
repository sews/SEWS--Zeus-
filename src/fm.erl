
-module(fm).
-export([getFile/1, getContents/1, getInfo/2, listDir/0]).

%% 			INTERNAL FUNCTIONS

listNonIndex() -> todo.
listIndex() -> todo.

getContentType(FileName) ->
	case string:sub_word(FileName, 2, $.) of
		"jpg" ->
			"image/jpeg";
		"txt" ->
			"text/plain";
		"html" ->
			"text/html";
		_ ->
			notype
	end.
	
getCharset (FileName) ->
	"UTF-8".

getFileInfo(FileName) ->
	[	{contenttype, getContentType(FileName)},
		{charset, getCharset(FileName)}		].

getFileLines(IOstream, Acc) ->
	case io:get_line(IOstream, "Prompt> ") of
		eof ->
			{ok, Acc};
		{error, Reason} ->
			{error, Reason};
		Data ->
			getFileLines(IOstream, Acc ++ Data)
	end.
	
	
%%			EXPORTED FUNCTIONS
%%

listDir() -> todo.


%%	getFile (FileName)
%%	Type	string -> {ok, filehandle} | {error, string}
%%
%%	Returns: {ok, FileHandle} if succesfull, {error, Reason} if not. Use FileHandle in all other file functions in this module.
%%
%%	Notes:	A filehandle has the form of {File, Info}. File is a string containing the entire file.
%%			Info contains any additional info about the file. This can be:
%%
%%			{contentType, 	<string>}		example: "image/jpeg"
%%			{charset,		<string>}	

getFile(FileName) -> 
	Options = [read],

	case file:open(FileName, Options) of
		{ok, IOstream} ->
			{_, Lines} = getFileLines(IOstream, []),
			Info  = getFileInfo(FileName),
			file:close(IOstream),
			{ok, {Lines, Info}};
		ErrorTuple ->	%% {error, Reason}
			ErrorTuple
	end.


%% 	getContents (FileHandle)
%%	Type:	filehandle -> string
%%

getContents({FileList, _}) -> FileList.


%% getInfo (FileHandle, Info)
%%
%% Type:	filehandle * string -> string | false
%%
%% Returns:	The corresponding file info in filehandle FileHandle to the atom Info, or false if it is not found.
	
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

		
	
		

