APP_NAME = SEWS Erlang Web Server
SRC = src/
DOCDIR = doc/

# Default compiler and compiling options
CC = erlc -W

ERL = erl

# Default testing options
TESTOP = $(ERL) -s

# Beam file location
EBIN = ebin/

GOTO = cd $(EBIN)

.PHONY: run build test clean rebuild doc docs

# Runs when make is called without parameters
all: build test

build:
	$(ERL) -make

# Compile one specific .erl
%.erl: 
	$(CC) $(SRC)$*.erl

# Test
run: build
	$(GOTO); $(TESTOP) main

# Remove all .beam files discarding errors
clean:
	@echo Cleaning some beams...
	@rm -f $(SRC)*.beam $(SRC)*.dump
	@rm -f *.beam *.dump
	@rm -f $(EBIN)*.beam $(EBIN)*.dump

# Removes all .beam files and compiles new
rebuild: 
	clean build

# Generates Edoc
edoc:
	@echo Generating $(APP_NAME) documentation from srcs
	@erl -noshell -run edoc_run application "'$(APP_NAME)'" \
	'"."' '[{def,{vsn,"$(VSN)"}}, {doc, "doc/"}, {files, "src/"}]'

# Generates a single Edoc
# For appfiles
# docs: .erl
#	erl -noshell -run edoc_run application "'$@.erl'"\
#	'"."' '[{def,{vsn,"$(VSN)"}}]'

test: build
	@echo Running eUnit...
	erl -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop

$(EBIN)%.beam : src/%.erl Makefile
	$(CC) -o $(EBIN) $<
