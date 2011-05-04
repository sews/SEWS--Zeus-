socket.beam: socket.erl
	erlc -W $<
test: socket.beam
	erl -noshell -s socket main 80