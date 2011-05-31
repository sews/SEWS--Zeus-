%% Lägg alla tester i denna modul
%% Alla testfunktioner bör avslutas med _test
%% Exporta ingen funktion!
%% För att köra alla tester, skriv test:test() i någon annan modul

-module(test).
-export([]).

-include_lib("eunit/include/eunit.hrl").

mall_test() ->
	?_assertEqual(true, true).
