all:
	erl -make

typer:
	typer -I ./include -I ./src src/*

dialyzer:
	dialyzer -I ./include -I ./src src/*

clean:
	rm -f erl_crash.dump ebin/*.beam

.PHONY: all typer dialyzer
