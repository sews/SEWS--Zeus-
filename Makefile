APP_NAME = SEWS Erlang Web Server
SRC = src/
DOCDIR = doc/html/

# Default compiler and compiling options
CC = erlc -W

ERL = erl

# Default testing options
TESTOP = $(ERL) -s

# Beam file location
EBIN = ebin/

GOTO = cd $(EBIN)

.PHONY: run build test clean rebuild edoc

# Runs when make is called without parameters
all: build test

build:
	$(ERL) -make

# Compile one specific .erl
%.erl: 
	$(CC) $(SRC)$*.erl

# KINDA WORKS
# ERROR AT 5
run: build
	$(GOTO); $(TESTOP) main

# Remove all .beam files discarding errors
clean:
	@echo Removing all beams and dumps...
	@rm -f $(SRC)*.beam $(SRC)*.dump $(SRC).#*
	@rm -f *.beam *.dump
	@rm -f $(EBIN)*.beam $(EBIN)*.dump
	@echo Removing doc
	@rm -f $(DOCDIR)*.html
	(cd doc/html && find . -name "*" -a ! -name overview.edoc -exec rm -rf {} \;)

# Removes all .beam files and compiles new
rebuild: clean build

# Generates Edoc
edoc:
	@echo Generating $(APP_NAME) documentation from srcs
	@erl -noshell -run edoc_run application "'$(APP_NAME)'" \
	'"."' '[{def,{vsn,"$(VSN)"}}, {dir, "$(DOCDIR)"}, {files, "$(SRC)"}]'

test: build
	@echo Running eUnit...
	erl -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop

$(EBIN)%.beam : src/%.erl Makefile
	$(CC) -o $(EBIN) $<
