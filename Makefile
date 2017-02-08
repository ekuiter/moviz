.DEFAULT_GOAL := all

all:
	rm *.dx64fsl || true
	echo "(ccl:save-application \"movie-graph\" :prepend-kernel t))" | ccl -e "(progn (load \"~/quicklisp/setup.lisp\") (load \"packages.lisp\"))"

run: all
	./movie-graph
	
clean:
	rm movie-graph || true
	
.PHONY: all run clean
