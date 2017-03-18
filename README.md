## moviz for macOS

![moviz](https://raw.githubusercontent.com/ekuiter/moviz/img/graph-example.png)

__Download the current moviz release [here](https://github.com/ekuiter/moviz/releases/latest) - no compiling needed.__

Or follow these instructions to build moviz yourself:

### Requirements

- [Clozure Common Lisp](http://ccl.clozure.com/) and [Quicklisp](https://www.quicklisp.org) (only compile-time, `brew install clozure-cl`)
- libuv (`brew install libuv`)
- the IMDb files (see below)
- a [prebuilt binary](https://github.com/electron/electron/releases) of Electron

### Getting started
Just follow these steps: _(`repo/` refers to this repository's root folder.)_

1. Download ftp://ftp.fu-berlin.de/pub/misc/movies/database/ to the `repo/server/imdb` directory
(as of now, the following files are required by the GUI: `actors.list`, `actresses.list`, `movies.list`, `trivia.list`, `goofs.list`, `quotes.list`, `soundtracks.list`, `crazy-credits.list` and `alternate-versions.list`).
2. Copy `Electron.app` to `repo/moviz.app`.
3. Run `make`.
4. Start moviz by double-clicking `repo/moviz.app`.

#### Stand-alone bundle

The `moviz.app` you just created is not stand-alone by default. It relies on files inside the `repo/server` directory. (This is useful for debugging. The app is also small in size because the IMDb files are not bundled within.)
If you want to distribute an app bundle that can run on its own, run `make bundle` in step 3 instead. A directory will be opened - copy the required (see step 1) IMDb list files into it. Your bundle is ready to use at `repo/moviz-bundle.app`.

### Server internals

This is for the interested reader who wants to learn more about how moviz works.
moviz consists of two parts: A Lisp server that provides all the "business logic" (searching IMDb files, creating & filtering graphs, generating SVG files, ...) and a JavaScript client responsible for the user interface. For convenience, both are bundled together in an [Electron](https://electron.atom.io/) app.
Note however, that you can use these parts separately:

- `make run-server` will start up a web server on `localhost:3000` accessible with your web browser of choice.
- `make run-repl` will start a REPL in which you can directly use all functionality exposed by the server and more.

#### The server

The server accepts the following GET requests: (Almost all routes return JSON data.)
- `/` displays the web interface
- `/setup/` fetches some TMDb data and needs to be called the first time the server is used
- `/graph/nodes/` returns all graph nodes
- `/graph/edges/` returns all graph edges
- `/tmdb/search/movies/MOVIE` fetches TMDb metadata for MOVIE
- `/tmdb/search/actors/ACTOR` fetches TMDb metadata for ACTOR
- `/clear/` clears the graph
- `/add/MOVIE-1/MOVIE-2/...` adds MOVIES to the graph
- `/abort/` aborts an add operation
- `/progress/` returns progress of add operation
- `/update/GRAPH-CLASS-1/GRAPH-CLASS-2/...` styles the graph according to GRAPH-CLASSES
- `/filter/node/FILTER` filters graph nodes using FILTER (given as JSON)
- `/filter/edge/FILTER` filters graph edges using FILTER (given as JSON)
- `/search/LIST/ID` searches IMDb LIST for ID
- `/inverse-search/LIST/RECORD` inverse searches IMDb LIST for RECORD
- `/details/MOVIE` fetches TMDb metadata and searches IMDb notes lists for MOVIE
- `/suggest/TITLE` suggests movies matching TITLE
- `/eval/FORM` evaluates FORM (given as string) on the server
- `/graph/save/` encodes the graph as a JSON file and downloads it
- `/graph/load/FILE-NAME` decodes the graph from the JSON file given by FILE-NAME
- `/graph/export/` creates a PNG file of the graph and downloads it

Some examples:
- `http://localhost:3000/search/actors/Amell, Stephen`
- `http://localhost:3000/inverse-search/actresses/Harry Potter and the Chamber of Secrets`
- `http://localhost:3000/search/trivia/Supernatural`

#### The REPL

Here's an example on how to generate and save a graph image from inside the REPL:

```
? (in-package :app)
#<Package "APP">
? (add-movies "House of Cards" "Person of Interest" "Westworld" "Supernatural" "Mr. Robot")
Information courtesy of IMDb (http://www.imdb.com). Used with permission.
Inverse searching imdb/actors.list for five movies ...
 0%  7% 16% 26% 35% 45% 55% 64% 74% 83% 93%
Inverse searching imdb/actresses.list for five movies ...
 0% 11% 27% 43% 59% 75% 91%
#<MOVIE-GRAPH {#<MOVIE-NODE Mr. Robot>, #<MOVIE-NODE Supernatural>, #<MOVIE-NODE Westworld>, #<MOVIE-NODE Person of Interest>, #<MOVIE-NODE House of Cards>}, 162 edges>
? (show (make-graph *graph* top-actors weighted))
NIL
? (save-and-quit "my-graph")
```

To open the graph again, simply run `./my-graph`.

##### Other features

Search for all movies an actor played in:
```
? (summarize (do-search (make-list-instance 'actors) (make-instance 'actor :name "Amell, Stephen")))
#<ROLE Amell, Stephen in [...]
```

Search for all actresses starring in a movie:
```
? (summarize (inverse-search (make-list-instance 'actresses) (make-instance 'movie :title "Harry Potter and the Chamber of Secrets")))
Inverse searching imdb/actresses.list for one movie ...
 0% 11% 27% 43% 59% 75% 91% 
#<ROLE Bates, Daisy (I) in [...]
```

Search for `alternate-versions`, `crazy-credits`, `goofs`, `soundtracks`, `trivia` and `quotes`:
```
? (summarize (do-search (make-list-instance 'trivia) (make-instance 'movie :title "Supernatural")))
Supernatural
============
SPOILER: Early on in the series, [...]
```

Run some tests:
```
? (tests:run-tests)
Testing MOVIES-LIST-SUGGEST ...
Testing QUOTES-LIST-DO-SEARCH ...
Testing TRIVIA-LIST-DO-SEARCH ...
Testing SOUNDTRACKS-LIST-DO-SEARCH ...
Testing GOOFS-LIST-DO-SEARCH ...
Testing CRAZY-CREDITS-LIST-DO-SEARCH ...
Testing ALTERNATE-VERSIONS-LIST-DO-SEARCH ...
Testing ACTORS-LIST-INVERSE-SEARCH ...
Inverse searching imdb/actresses.list for one movie ...
 0% 11% 27% 43% 59% 75% 91% 100%
Testing ACTORS-LIST-DO-SEARCH ...
9 of 9 tests passed.
```

© Elias Kuiter 2017 - elias-kuiter.de
