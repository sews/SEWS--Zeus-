#
#
SRC = src/
# Default compiler and compiling options
CC = erlc -W
# Default testing options
TESTOP = erl -noshell -s

# Compile all .erl files older than respective .beam file
build: $(SRC)*.erl
	$(CC) $?

# GER VARNINGAR, FIX IT!
# Compile one specific .erl
#.beam: $@.erl
#	$(CC) $@.erl

# Compile socket.erl
#.beam: $@.erl
#	$(CC)

# Test
test: sewsmain.beam
	$(TESTOP) sewsmain main 80

# Remove all .beam files discarding errors
.PHONY: clean
clean:
	rm -f $(SRC)*.beam

# Removes all .beam files and compiles new
rebuild: 
	clean build
