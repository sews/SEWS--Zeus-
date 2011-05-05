
-module(filemanager).
-export([getFile/1, listDir/0]).


listNonIndex() -> [].
listIndex() -> [].

listDir() -> [].

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
			{ok, lists:reverse(Acc)};
		{error, Reason} ->
			{error, Reason};
		Data ->
			getFileLines(IOstream, [Data | Acc])
	end.
	

%%	getFile (fileName)
%%	Type	string -> {ok, string list, 2-Tuple list} | {error, string}
%%
%%	Notes:	If file is found returns {ok, File, Info}. File is a list where each entry is one line from the file.
%%			Info contains any additional info about the file. This can be:
%%			{contentType, <string>}		example: "image/jpeg"
%%			{}	
%%
%%
%%

getFile(FileName) -> 

	Options = [read],

	case file:open(FileName, Options) of
		{ok, IOstream} ->
			{_, Lines} = getFileLines(IOstream, []),
			Info  = getFileInfo(FileName),
			file:close(IOstream),
			{ok, Lines, Info};
		ErrorTuple ->	%% {error, Reason}
			ErrorTuple
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

		
	
		

