.DEFAULT_GOAL := all

all:
	elm-make *.elm --yes --warn --output=../assets/elm.js

clean:
	rm ../assets/elm.js || true

.PHONY: all clean
