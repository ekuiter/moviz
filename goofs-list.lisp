(in-package :imdb)

(defclass goof (movie-information) ())
(defclass goofs-list (movie-information-list) ())

(define-data-bound-slot goofs-list start "GOOFS LIST" 2)

(defmethod movie-information-class ((goofs-list goofs-list))
  'goof)