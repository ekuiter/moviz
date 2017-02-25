(in-package :imdb)

(defclass alternate-version (movie-information) ())
(defclass alternate-versions-list (movie-information-list) ())

(define-data-bound-slot alternate-versions-list start "ALTERNATE VERSIONS LIST" 3)
(define-data-bound-slot alternate-versions-list end "SUBMITTING NEW DATA" 11)

(defmethod record-line-p ((av-list alternate-versions-list) line)
  (and (> (length line) 0) (or (char= (aref line 0) #\#)
			       (equal (subseq line 0 2) "--"))))

(defmethod movie-information-class ((av-list alternate-versions-list))
  'alternate-version)