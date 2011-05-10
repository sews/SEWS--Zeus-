#
# Source folder
SRC = src/

# Doc folder
DOCDIR = doc/

# Default compiler and compiling options
CC = erlc -W

# Erlang
ERL = erl

# Default testing options
TESTOP = $(ERL) -noshell -s

# All files
# Deprecated
SRCFILES = $(SRC)sewsmain.erl $(SRC)sewsparser.erl $(SRC)fm.erl $(SRC)get.erl $(SRC)gen_html.erl $(SRC)error_mod.erl

.PHONY: build test clean rebuild doc docs

# Runs when make is called without parameters
all: build

# Kompilerar alla filer, oavsett ålder..
# Borde endast kompilera de som är
# äldre än respektive .beam fil då :*.erl.

# TODO => Compile all .erl files older than respective .beam file
build:
	$(ERL) -make

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
clean:
	rm -f $(SRC)*.beam
	rm -f *.beam
	rm -f ebin/*.beam

# Removes all .beam files and compiles new
rebuild: 
	clean build

# Gör denna, Edoc ska genereras automatiskt
# Generates Edoc
doc: $(SRCFILES)
	make $(DOCDIR)*.html

# Generates a single Edoc
# For appfiles
# docs: .erl
#	erl -noshell -run edoc_run application "'$@.erl'"\
#	'"."' '[{def,{vsn,"$(VSN)"}}]'
