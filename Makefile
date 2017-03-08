.DEFAULT_GOAL := all

all:
	rm *.dx64fsl || true
	echo "(ccl:save-application \"moviz\" :prepend-kernel t))" | ccl64 -e "(progn (load \"~/quicklisp/setup.lisp\") (load \"packages.lisp\"))"

run: all
	./moviz

clean:
	rm moviz || true

.PHONY: all run clean
