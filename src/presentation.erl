
-module(presentation).

-export([start/0]).

newLine(0) -> " ";
newLine(N) -> io:nl(), newLine(N-1).

enter(MegaString) -> io:get_line("Press enter to continue>"), 
		   newLine(100),
		   io:format(MegaString, []).

startAux([], Acc) -> "END OF TRANSMISSION";
startAux([H | Rest], Acc) -> 
	io:format(H, []),
	enter(Acc ++ H),
	startAux(Rest, Acc ++ H).


start () ->
	newLine(100),
	L = ["~nWelcome to The SEWS Demo Presentation!~n~n",
	"INTRO:~nTo start SEWS enter main:start() in the erlang shell.~n~n1>main:start().~n<0.37.0>~n~n",
	"The main module listens to a port (default port 8080).~n",
	"When a request is sent to SEWS, main sends the request to the parser module.~n",
	"~nPARSER:~nThe parser module extracts data from the request into a manageable format.~n",
	"~n1>Parsed = parser:parse(binary_to_list(Indata)).~n~n",
	"~nHere's an example of Indata (a GET request sent from a web site using a GET form):~n
GET /dennisrosen HTTP/1.1\r\nHost: localhost:8080\r\nUser-Agent: Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.1.3) Gecko/20091020 Ubuntu/9.10 (karmic) Firefox/3.5.3\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-us,en;q=0.5\r\nAccept-Encoding: gzip,deflate\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 300\r\nConnection: keep-alive\r\n\r\n",
	"~nAnd here's the result of parser:parse():~n
{get,[{connection,\"keep-alive\"},
      {keep_alive,[\"300\"]},
      {accept_charset,[\"ISO-8859-1,utf-8;q=0.7,*;q=0.7\"]},
      {accept_encoding,[\"gzip,deflate\"]},
      {accept_language,[\"en-us,en;q=0.5\"]},
      {user_agent,[\"Mozilla/5.0\",\"(X11;\",\"U;\",\"Linux\",\"x86_64;\",\"en-US;\",
                   \"rv:1.9.1.3)\",\"Gecko/20091020\",\"Ubuntu/9.10\",\"(karmic)\",
                   \"Firefox/3.5.3\"]},
      {host,\"localhost:8080\"},
      {path,\"/dennisrosen/\"}]}~n~n",
    "GET:~nThe parsed request is then sent towards the GET or POST module depending upon its type.~n~nget:handler(Parsed).~n~n",
    "The GET module extracts the path, filename and other info and does work accordingly. If the file requested is a folder, GET first searches for a index.html file in the folder, and returns its contents if found.~n",
	"If index.html does not exist, GET sends the request on towards the gen_html module, which generates an html document listing the files in the folder.~n",
	"If the request is an actual file and not a folder, the file contents are returned from the GET module.~n",
	"~nFM and Cache~nGET uses the fm module (filemanager) to read, search and manipulate files. The fm module in turn uses the cache module to cache files read.~n",
	"The cache module implements by default an LRU algorithm for file caching. The caching algorithm used is fully customizable, as is the maximum cache size.~n",
	"~nDynerl~nSEWS also supports custom scripting in special files suffixed with \".esl\".~n",
	"This means SEWS is able to read *.esl files and execute any code in the file surrounded by the tags <?esl and ?>.~n",
	"Example: The file \"big alex on horse.esl\":~n~nalex~n<?erl~nA = 1 + 2,~nC = A.~n?>on~nhorse~n~n",
	"When a GET request for this file is recieved SEWS will execute the erlang code.~n"
	],
	
	startAux(L, []).
	
	
	
	
	
	
	
