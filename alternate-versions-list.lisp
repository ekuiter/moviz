(in-package :imdb)

(defclass alternate-versions-list (imdb-list) ())

(define-data-bound-slot alternate-versions-list start "ALTERNATE VERSIONS LIST" 4)
(define-data-bound-slot alternate-versions-list end "SUBMITTING NEW DATA" 11)

(defmethod record-line-p ((av-list alternate-versions-list) line)
  (and (> (length line) 0) (or (char= (aref line 0) #\#)
			       (equal (subseq line 0 2) "--"))))

(defmethod read-until-record ((av-list alternate-versions-list) stream &key)
  (call-next-method av-list stream
		    :extract-id-fn (lambda (line)
				     (cl-ppcre:register-groups-bind (title episode)
					 ("# (.*?) \\([\\d?]{4}(?:.*\\{(.*?)\\})?" line)
				       (values title episode)))))

(defmethod read-record ((av-list alternate-versions-list) movie stream &key)
  (flet ((extract-record-fn (record)
	   (subseq record (position #\Newline record))))
    (call-next-method av-list movie stream :extract-record-fn #'extract-record-fn)))

(defmethod do-search ((av-list alternate-versions-list) (movie movie) &key)
  (call-next-method av-list movie :id (title movie)))