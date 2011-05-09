#
# Source folder
SRC = src/

# Doc folder
DOC = doc/

# Default compiler and compiling options
CC = erlc -W

# Default testing options
TESTOP = erl -noshell -s

# Runs when make is called without parameters
all: build

# Kompilerar alla filer, oavsett �lder..
# Borde endast kompilera de som �r
# �ldre �n respektive .beam fil d� :*.erl.

#TODO => Compile all .erl files older than respective .beam file
build: $(SRC)*.erl
	$(CC) $?

# Compile one specific .erl
%.erl: 
	$(CC) $(SRC)$*.erl

# Compile socket.erl
#.beam: $@.erl
#	$(CC)

# Beh�ver ut�kas?
# Test
test: build
	$(TESTOP) sewsmain start 8888

# Remove all .beam files discarding errors
.PHONY: clean
clean:
	rm -f $(SRC)*.beam
	rm -f *.beam

# Removes all .beam files and compiles new
rebuild: 
	clean build

# G�r denna, Edoc ska genereras automatiskt
# Generates Edoc
#doc: 
#	edoc:files([])
