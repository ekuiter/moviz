## movie-graph

Generates graphs like this:
![Graph Example](https://raw.githubusercontent.com/ekuiter/movie-graph/img/graph-example.png)

### Getting started

Download ftp://ftp.fu-berlin.de/pub/misc/movies/database/ to the `imdb` directory
(`actors.list` and `actresses.list` in particular).
Run with `make run`, Clozure Common Lisp required. Then:

```
? (in-package :main)
#<Package "MAIN">
? (add-movies "House of Cards" "Person of Interest" "Westworld" "Supernatural" "Mr. Robot")
Information courtesy of IMDb (http://www.imdb.com). Used with permission.
Inverse searching imdb/actors.list for five movies ...
 0%  7% 16% 26% 35% 45% 55% 64% 74% 83% 93%
Inverse searching imdb/actresses.list for five movies ...
 0% 11% 27% 43% 59% 75% 91%
#<MOVIE-GRAPH {#<MOVIE-NODE Mr. Robot>, #<MOVIE-NODE Supernatural>, #<MOVIE-NODE Westworld>, #<MOVIE-NODE Person of Interest>, #<MOVIE-NODE House of Cards>}, 162 edges>
? (show *graph* :condensed t)
NIL
? (save-and-quit "my-graph")
```

To open the graph again, simply run `./my-graph`.

© Elias Kuiter 2017 - elias-kuiter.de