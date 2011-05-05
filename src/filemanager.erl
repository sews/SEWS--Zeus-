
-module(filemanager).
-export([getFile/1, listDir/0]).


listNonIndex() ->
listIndex() ->

listDir() ->

getContentType(fileName) ->
	case string:sub_word(fileName, 1, $.) of
		"jpg" ->
			"image/jpeg";
		"txt" ->
			"text/plain";
		"html" ->
			"text/html"
		Rest ->
			notype
	end.
	
getCharset (fileName) ->
	"UTF-8".

getFileInfo(fileName) ->
	[	{contenttype, getContentType(fileName)},
		{charset, getCharset(fileName)}		].

getFileLines(iostream) ->
	case io:get_line(iostream) of
		{ok, Data} ->
			[Data | getFileLines(iostream)];
		eof ->
			[];
		{error, Reason} ->  {ok, File, Info} ->
			hej	
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

getFile(fileName) -> 
	case file:open(fileName, options) of
		{ok, IOstream, getFileInfo(fileName)} ->
			{ok, getFileLines(IOstream), getFileInfo(fileName)};
		ErrorTuple ->
			ErrorTuple
	end.

