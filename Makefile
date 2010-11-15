ERL = erl
ERLC = erlc

GENFILES = src/gdsdump_lex.erl

all: $(GENFILES)
	$(ERL) -make

typer: $(GENFILES)
	typer -I ./include -I ./src src/*.erl

dialyzer: $(GENFILES)
	dialyzer -I ./include -I ./src src/*.erl

clean:
	rm -f erl_crash.dump ebin/*.beam $(GENFILES)

%.erl: %.xrl
	$(ERLC) +debug_info -o $(dir $<) $<

.PHONY: all typer dialyzer clean
