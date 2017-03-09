(in-package :imdb)

(defclass notes-list (imdb-list) ())
(defclass notes-list-with-end (notes-list) ())

(define-lazy-slot notes-list data-end
    "Returns the offset after the last record."
    (file-length instance))

(defmethod id-class ((notes-list notes-list)) 'movie)

(defmethod record-line-p ((notes-list notes-list) line)
  (and (> (length line) 0) (char= (aref line 0) #\#)))

(defmethod record-line-p ((list notes-list-with-end) line)
  (and (> (length line) 0) (or (char= (aref line 0) #\#)
			       (equal (subseq line 0 2) "--"))))

(defun extract-notes-list-record-id (line)
  "Extracts the movie title from a record's first line."
  (let ((results (cl-ppcre:register-groups-bind (title episode)
		     ("# (.*?) \\([\\d?]{4}(?:.*\\{(.*?)\\})?" line)
		   (list title episode))))
    (if results (apply #'values results) (subseq line 2))))

(defmethod read-until-record ((notes-list notes-list) stream &key)
  (call-next-method notes-list stream :extract-id-fn #'extract-notes-list-record-id))

(defun char-to-string (char)
  "Returns a string with only the given character."
  (make-string 1 :initial-element char))

(defmethod read-record ((notes-list notes-list) movie stream
			&key (delimiter "") extract-notes-fn include-empty-lines)
  (unless extract-notes-fn
    (setf extract-notes-fn
	  (lambda (notes)
	    (setf notes (cl-ppcre:regex-replace-all "\\n " notes (char-to-string #\Tab)))
	    (setf notes (split-sequence #\Newline notes :remove-empty-subseqs t))
	    (mapcar (lambda (note) (cl-ppcre:regex-replace-all "\\t" (subseq note 2)
							       delimiter)) notes))))
  (flet ((extract-record-fn (record)
	   (let* ((new-line-pos (position #\Newline record))
		  (notes (subseq record new-line-pos)))
	     (multiple-value-bind (title episode)
		 (extract-notes-list-record-id (subseq record 0 new-line-pos))
	       (declare (ignore title))
	       (make-instance (record-class notes-list)
			      :movie movie :episode episode
			      :info (funcall extract-notes-fn notes))))))
    (call-next-method notes-list movie stream :extract-record-fn #'extract-record-fn
		      :include-empty-lines include-empty-lines)))

(defmethod do-search ((notes-list notes-list) (movie movie))
  (do-search-linear notes-list movie :id (title movie)
		    :id= (lambda (id current-id) (equal id (string-trim "\"" current-id)))))

(defclass alternate-versions (movie-record) ())
(defclass alternate-versions-list (notes-list-with-end) ())
(define-data-bound-slot alternate-versions-list start "ALTERNATE VERSIONS LIST" 3)
(define-data-bound-slot alternate-versions-list end "SUBMITTING NEW DATA" 11)
(defmethod record-class ((av-list alternate-versions-list)) 'alternate-versions)

(defclass crazy-credits (movie-record) ())
(defclass crazy-credits-list (notes-list-with-end) ())
(define-data-bound-slot crazy-credits-list start "CRAZY CREDITS" 1)
(let ((end-delimiter (format nil "~79@{-~}" nil)))
  (define-data-bound-slot crazy-credits-list end end-delimiter 1))
(defmethod record-class ((cc-list crazy-credits-list)) 'crazy-credits)

(defclass goofs (movie-record) ())
(defclass goofs-list (notes-list) ())
(define-data-bound-slot goofs-list start "GOOFS LIST" 2)
(defmethod record-class ((goofs-list goofs-list)) 'goofs)

(defclass soundtracks (movie-record) ())
(defclass soundtracks-list (notes-list-with-end) ())
(define-data-bound-slot soundtracks-list start "SOUNDTRACKS" 1)
(let ((end-delimiter (format nil "~79@{-~}" nil)))
  (define-data-bound-slot soundtracks-list end end-delimiter 1))
(defmethod record-class ((soundtracks-list soundtracks-list)) 'soundtracks)
(defmethod read-record ((soundtracks-list soundtracks-list) movie stream &key)
  (call-next-method soundtracks-list movie stream :delimiter (char-to-string #\Newline)))

(defclass trivia (movie-record) ())
(defclass trivia-list (notes-list) ())
(define-data-bound-slot trivia-list start "FILM TRIVIA" 2)
(defmethod record-class ((trivia-list trivia-list)) 'trivia)

(defclass quotes (movie-record) ())
(defclass quotes-list (notes-list) ())
(define-data-bound-slot quotes-list start "QUOTES LIST" 1)
(let ((end-delimiter (format nil "~80@{-~}" nil)))
  (define-data-bound-slot quotes-list end end-delimiter 1))
(defmethod record-class ((quotes-list quotes-list)) 'quotes)
(defmethod read-record ((quotes-list quotes-list) movie stream &key)
  (flet ((extract-notes-fn (notes)
	   (setf notes (cl-ppcre:regex-replace-all "\\n\\n" notes (char-to-string #\Tab)))
	   (setf notes (split-sequence #\Tab notes :remove-empty-subseqs t))
	   (setf notes (mapcar (lambda (note) (string-left-trim (list #\Newline) note)) notes))
	   (remove "" notes :test #'equal)))
  (call-next-method quotes-list movie stream
		    :extract-notes-fn #'extract-notes-fn :include-empty-lines t)))
(defmethod summary ((quotes quotes) &key)
  (call-next-method quotes :suffix (char-to-string #\Newline)))
