%% Default port for SEWS to listen on
%%
-define(DEFAULT_PORT, 8080).

%% WEB ROOT FOR SEWS (usually /var/www)
%% NO TRAILING SLASH
%%
-define(WWW_ROOT, "/home").

%% index file, no need for an explanation, eh?
%%
-define(INDEX_FILE, "index.html").

%% cache stuff. Don't touch unless you know what you do
%%
-define(ETS_OPTIONS,[set,named_table,public]).
-define(MAX_FILE_SIZE,2000000).
-define(MAX_CACHE_SIZE,51).

%% TCP Options.
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
