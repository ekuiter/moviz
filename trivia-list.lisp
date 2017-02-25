(in-package :imdb)

(defclass trivia (movie-information) ())
(defclass trivia-list (movie-information-list) ())

(define-data-bound-slot trivia-list start "FILM TRIVIA" 2)

(defmethod movie-information-class ((trivia-list trivia-list))
  'trivia)