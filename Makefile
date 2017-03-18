.DEFAULT_GOAL := all

CCL_EVAL = "(progn (load \"~/quicklisp/setup.lisp\") (load \"packages.lisp\") (setf ccl:*break-hook* (lambda (cond hook) (declare (ignore cond hook)) (ccl:quit))))"

repl:
	cd server; echo $(shell pwd); \
	rm *.dx64fsl || true; \
	echo "(ccl:save-application \"moviz-repl\" :prepend-kernel t)" | ccl64 -e $(CCL_EVAL)

server:
	cd server; echo $(shell pwd); \
	rm *.dx64fsl || true; \
	echo "(ccl:save-application \"moviz-server\" :prepend-kernel t :toplevel-function #'server:serve)" | ccl64 -e $(CCL_EVAL)

client: server
	if [ ! -d "moviz.app" ]; then echo "ERROR: Please provide a clean Electron.app as moviz.app!"; exit 1; fi
	chmod +x client/Contents/MacOS/run
	cp -R client/ moviz.app/
	rm moviz.app/Contents/MacOS/moviz-server || true
	rm moviz.app/Contents/MacOS/assets || true
	rm moviz.app/Contents/MacOS/imdb || true
	ln -s $(shell pwd)/server/moviz-server moviz.app/Contents/MacOS/moviz-server
	ln -s $(shell pwd)/server/assets moviz.app/Contents/MacOS/
	ln -s $(shell pwd)/server/imdb moviz.app/Contents/MacOS/
	xattr -rd com.apple.quarantine moviz.app

all: client

bundle: client
	rm -r moviz-bundle.app || true
	cp -R moviz.app moviz-bundle.app
	rm moviz-bundle.app/Contents/MacOS/moviz-server || true
	rm moviz-bundle.app/Contents/MacOS/assets || true
	rm moviz-bundle.app/Contents/MacOS/imdb || true
	cp server/moviz-server moviz-bundle.app/Contents/MacOS/moviz-server
	cp -R server/assets moviz-bundle.app/Contents/MacOS/
	rm moviz-bundle.app/Contents/MacOS/assets/graph.svg || true
	rm moviz-bundle.app/Contents/MacOS/assets/graph.png || true
	rm moviz-bundle.app/Contents/MacOS/*.dat || true
	mkdir moviz-bundle.app/Contents/MacOS/imdb
	echo "CAUTION: Remember to copy the IMDb files into this folder before using moviz!" > moviz-bundle.app/Contents/MacOS/imdb/README
	open moviz-bundle.app/Contents/MacOS/imdb

run: client
	open moviz.app

run-repl: repl
	cd server; ./moviz-repl

run-server: server
	cd server; ./moviz-server

clean:
	rm server/moviz-repl || true
	rm server/moviz-server || true

.PHONY: repl server client all bundle run run-repl run-server clean
