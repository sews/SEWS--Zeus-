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
SRCFILES = $(SRC)$(wildcard *.erl)

# Beam file location
EBIN = ebin/

# All .beam files
BEAMFILES = $(EBIN)$(SRCFILES: .erl = .beam)

.PHONY: run build test clean rebuild doc docs

# Runs when make is called without parameters
all: build

# Kompilerar alla filer, oavsett �lder..
# Borde endast kompilera de som �r
# �ldre �n respektive .beam fil d� :*.erl.

# TODO => Compile all .erl files older than respective .beam file
build:
	$(ERL) -make

# Compile one specific .erl
%.erl: 
	$(CC) $(SRC)$*.erl

# Compile socket.erl
#.beam: $@.erl
#	$(CC)

# Beh�ver ut�kas?
# Test
run: build
	$(TESTOP) sewsmain start 8888

# Remove all .beam files discarding errors
clean:
	rm -f $(SRC)*.beam
	rm -f *.beam
	rm -f ebin/*.beam

# Removes all .beam files and compiles new
rebuild: 
	clean build

# G�r denna, Edoc ska genereras automatiskt
# Generates Edoc
doc: $(SRCFILES)
	make $(DOCDIR)*.html

# Generates a single Edoc
# For appfiles
# docs: .erl
#	erl -noshell -run edoc_run application "'$@.erl'"\
#	'"."' '[{def,{vsn,"$(VSN)"}}]'

test: build
	erl -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop
