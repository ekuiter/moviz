(in-package :imdb)

(defclass movies-list (imdb-list) ())

(define-data-bound-slot movies-list start "MOVIES LIST" 2)
(let ((end-delimiter (format nil "~80@{-~}" nil)))
  (define-data-bound-slot movies-list end end-delimiter 1))

(defmethod id-class ((movies-list movies-list)) 'movie)
(defmethod record-class ((movies-list movies-list)) 'movie-record)

(defmethod record-line-p ((movies-list movies-list) line)
  (> (length line) 0))

(defun extract-movies-list-record-id (line)
  "Extracts the movie title from a record's first line."
  (let ((results (cl-ppcre:register-groups-bind (title episode)
		     ("(.*?) \\([\\d?]{4}(?:.*\\{(.*?)\\})?" line)
		   (list title episode))))
    (if results (apply #'values results) line)))

(defmethod read-until-record ((movies-list movies-list) stream &key)
  (call-next-method movies-list stream :extract-id-fn #'extract-movies-list-record-id))

(defmethod read-record ((movies-list movies-list) movie stream &key)
  (flet ((extract-record-fn (record)
	   (let* ((tab-pos (position #\Tab record))
		  (info (string-left-trim (list #\Tab) (subseq record tab-pos)))
		  (info (string-right-trim (list #\Newline) info)))
	     (multiple-value-bind (title episode)
		 (extract-movies-list-record-id (subseq record 0 tab-pos))
	       (declare (ignore title))
	       (make-instance (record-class movies-list)
			      :movie movie :episode episode :info info)))))
    (call-next-method movies-list movie stream :extract-record-fn #'extract-record-fn)))

(defmethod do-search ((movies-list movies-list) (movie movie))
  (do-search-linear movies-list movie :id (title movie)
		    :id= (lambda (id current-id) (equal id (string-trim "\"" current-id)))))

(defmethod do-search-suggest ((movies-list movies-list) title)
  (flet ((starts-with (id current-id)
	   (eql (search id current-id) 0)))
    (multiple-value-bind (record pos)
	(do-search-binary movies-list (make-instance 'movie :title title)
			  :id title :id= #'starts-with)
      (declare (ignore record))
      (let ((results nil) (back 100) (lines 1000))
	(when pos
	  (with-open-list movies-list
	    (file-position stream pos)
	    (handler-case (dotimes (i back) (back-line stream)) (type-error ()))
	    (file-position stream (max (data-start movies-list) (file-position stream)))
	    (do-lines stream ((i 0 (1+ i))) (or (null line) (= i lines) (= (length results) 10))
	      (multiple-value-bind (current-title episode) (extract-movies-list-record-id line)
		(declare (ignore episode))
		(when (starts-with title current-title)
		  (pushnew (string-trim "\"" current-title) results :test #'equal))))
	    (nreverse results)))))))

(defmethod suggest ((movies-list movies-list) title)
  (delete-duplicates-in-sorted-list
   (sort (nconc (do-search-suggest movies-list title)
		(do-search-suggest movies-list (format nil "\"~a\"" title)))
	 #'string<)
   :test #'equal))
