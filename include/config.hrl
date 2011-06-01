-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(DEFAULT_PORT, 8080).

%% WEB ROOT FOR SEWS (usually /var/www)
-define(WWW_ROOT, "/var/www").

%% index file
-define(INDEX_FILE, "index.html").


%% cache

-define(ETS_OPTIONS,[set,named_table,public]).
-define(MAX_FILE_SIZE,2000000).
-define(MAX_CACHE_SIZE,51).
