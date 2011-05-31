-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(DEFAULT_PORT, 8080).

%% WEB ROOT FOR SEWS (usually /var/www)
-define(WWW_ROOT, "/home").

%% index file
-define(INDEX_FILE, "index.html").
