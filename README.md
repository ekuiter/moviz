## movie-graph

Generates graphs like this:

![Graph Example](https://raw.githubusercontent.com/ekuiter/movie-graph/img/graph-example.png)

### Getting started

Download ftp://ftp.fu-berlin.de/pub/misc/movies/database/ to the `imdb` directory
(`actors.list` and `actresses.list` in particular).
Run with `make run`, Clozure Common Lisp required. Then:

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

### Other features

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
Testing QUOTES-LIST-DO-SEARCH ...
Testing TRIVIA-LIST-DO-SEARCH ...
Testing SOUNDTRACKS-LIST-DO-SEARCH ...
Testing GOOFS-LIST-DO-SEARCH ...
Testing CRAZY-CREDITS-LIST-DO-SEARCH ...
Testing ALTERNATE-VERSIONS-LIST-DO-SEARCH ...
Testing ACTORS-LIST-INVERSE-SEARCH ...
Inverse searching imdb/actresses.list for one movie ...
 0% 11% 27% 43% 59% 75% 91% 
Testing ACTORS-LIST-DO-SEARCH ...
8 of 8 tests passed.
```

Run a server with `(server:serve)`, then visit these URLs to run the examples from above:
- `http://localhost:3000/search/actors/Amell, Stephen`
- `http://localhost:3000/inverse-search/actresses/Harry Potter and the Chamber of Secrets`
- `http://localhost:3000/search/trivia/Supernatural`

© Elias Kuiter 2017 - elias-kuiter.de
