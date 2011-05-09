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

# Kompilerar alla filer, oavsett ålder..
# Borde endast kompilera de som är
# äldre än respektive .beam fil då :*.erl.

#TODO => Compile all .erl files older than respective .beam file
build: $(SRC)*.erl
	$(CC) $?

# Compile one specific .erl
%.erl: 
	$(CC) $(SRC)$*.erl

# Compile socket.erl
#.beam: $@.erl
#	$(CC)

# Behöver utökas?
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

# Gör denna, Edoc ska genereras automatiskt
# Generates Edoc
#doc: 
#	edoc:files([])
