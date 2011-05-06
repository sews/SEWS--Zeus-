#
# TODO:	Lägg in en PATH som prefix för alla kommandon/filer
#	Så make kan köras utanför src-katalogen.
#
# Add .erl files to ERL constant
# ERL =  

# Add .beam files to BEAMS constant
# BEAMS =  

# Add testfunctions here
# TEST = 

# Default compiler and compiling options
CC = erlc -W

# Default testing options
TestOp = erl -noshell -s

# Compile all .erl files older than respective .beam file
build: .erl
	$(CC) $?

# Compile one specific .erl
.beam: $@.erl
	$(CC) $@.erl

# Compile socket.erl
.beam: $@.erl
	$(CC)

# Test socket.beam
test: socket.beam
	$(TestOp) socket main 80

# Remove all .beam files discarding errors
.PHONY: clean
clean:
	rm -f *.beam

# Removes all .beam files and compiles new
rebuild: 
	clean build
