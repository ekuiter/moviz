(in-package :imdb)

(defclass notes-record ()
  ((movie :initarg :movie
	  :initform (error "Must supply movie")
	  :reader movie)
   (episode :initarg :episode
	    :initform nil
	    :reader episode)
   (notes :initarg :notes
	  :initform (error "Must supply notes")
	  :reader notes)))

(defclass notes-list (imdb-list) ())
(defclass notes-list-with-end (notes-list) ())

(define-lazy-slot notes-list data-end
    "Returns the offset after the last record."
    (file-length instance))

(defgeneric record-class (notes-list)
  (:documentation "Returns the associated record class."))

(defmethod print-object ((info notes-record) stream)
  "Prints a notes record."
  (print-unreadable-object (info stream :type t)
    (format stream "~a~@[ - ~a~]"
	    (title (movie info)) (episode info))))

(defmethod summary ((info notes-record) &key (suffix ""))
  "Returns notes record in a readable form."
  (let ((heading (format nil "~a~@[ - ~a~]" (title (movie info)) (episode info))))
    (format nil "~a~%~v@{=~}~%~{~a~%~}" heading (length heading)
	    (mapcar (lambda (note) (format nil "~a~a" note suffix)) (notes info)))))

(defmethod record-line-p ((notes-list notes-list) line)
  (and (> (length line) 0) (char= (aref line 0) #\#)))

(defmethod record-line-p ((list notes-list-with-end) line)
  (and (> (length line) 0) (or (char= (aref line 0) #\#)
			       (equal (subseq line 0 2) "--"))))

(defun extract-notes-record-id (line)
  "Extracts the movie title from a record's first line."
  (let ((results (cl-ppcre:register-groups-bind (title episode)
		     ("# (.*?) \\([\\d?]{4}(?:.*\\{(.*?)\\})?" line)
		   (list title episode))))
    (if results (apply #'values results) (subseq line 2))))

(defmethod read-until-record ((notes-list notes-list) stream &key)
  (call-next-method notes-list stream :extract-id-fn #'extract-notes-record-id))

(defun char-to-string (char)
  "Returns a string with only the given character."
  (make-string 1 :initial-element char))

(defmethod read-record ((notes-list notes-list) movie stream
			&key (delimiter "") extract-notes-fn include-empty-lines)
  (unless extract-notes-fn
    (setf extract-notes-fn
	  (lambda (notes)
	    (setf notes (cl-ppcre:regex-replace-all "\\n " notes (char-to-string #\Tab)))
	    (setf notes (split-sequence:split-sequence #\Newline notes :remove-empty-subseqs t))
	    (mapcar (lambda (note) (cl-ppcre:regex-replace-all "\\t" (subseq note 2)
							       delimiter)) notes))))
  (flet ((extract-record-fn (record)
	   (let* ((new-line-pos (position #\Newline record))
		  (notes (subseq record new-line-pos)))
	     (multiple-value-bind (title episode)
		 (extract-notes-record-id (subseq record 0 new-line-pos))
	       (declare (ignore title))
	       (make-instance (record-class notes-list)
			      :movie movie :episode episode
			      :notes (funcall extract-notes-fn notes))))))
    (call-next-method notes-list movie stream :extract-record-fn #'extract-record-fn
		      :include-empty-lines include-empty-lines)))

(defmethod do-search ((notes-list notes-list) (movie movie))
  (do-search-linear notes-list movie :id (title movie)
		    :id= (lambda (id current-id) (equal id (string-trim "\"" current-id)))))

(defclass alternate-versions (notes-record) ())
(defclass alternate-versions-list (notes-list-with-end) ())
(define-data-bound-slot alternate-versions-list start "ALTERNATE VERSIONS LIST" 3)
(define-data-bound-slot alternate-versions-list end "SUBMITTING NEW DATA" 11)
(defmethod record-class ((av-list alternate-versions-list)) 'alternate-versions)

(defclass crazy-credits (notes-record) ())
(defclass crazy-credits-list (notes-list-with-end) ())
(define-data-bound-slot crazy-credits-list start "CRAZY CREDITS" 1)
(let ((end-delimiter (format nil "~79@{-~}" nil)))
  (define-data-bound-slot crazy-credits-list end end-delimiter 1))
(defmethod record-class ((cc-list crazy-credits-list)) 'crazy-credits)

(defclass goofs (notes-record) ())
(defclass goofs-list (notes-list) ())
(define-data-bound-slot goofs-list start "GOOFS LIST" 2)
(defmethod record-class ((goofs-list goofs-list)) 'goofs)

(defclass soundtracks (notes-record) ())
(defclass soundtracks-list (notes-list-with-end) ())
(define-data-bound-slot soundtracks-list start "SOUNDTRACKS" 1)
(let ((end-delimiter (format nil "~79@{-~}" nil)))
  (define-data-bound-slot soundtracks-list end end-delimiter 1))
(defmethod record-class ((soundtracks-list soundtracks-list)) 'soundtracks)
(defmethod read-record ((soundtracks-list soundtracks-list) movie stream &key)
  (call-next-method soundtracks-list movie stream :delimiter (char-to-string #\Newline)))

(defclass trivia (notes-record) ())
(defclass trivia-list (notes-list) ())
(define-data-bound-slot trivia-list start "FILM TRIVIA" 2)
(defmethod record-class ((trivia-list trivia-list)) 'trivia)

(defclass quotes (notes-record) ())
(defclass quotes-list (notes-list) ())
(define-data-bound-slot quotes-list start "QUOTES LIST" 1)
(let ((end-delimiter (format nil "~80@{-~}" nil)))
  (define-data-bound-slot quotes-list end end-delimiter 1))
(defmethod record-class ((quotes-list quotes-list)) 'quotes)
(defmethod read-record ((quotes-list quotes-list) movie stream &key)
  (flet ((extract-notes-fn (notes)
	   (setf notes (cl-ppcre:regex-replace-all "\\n\\n" notes (char-to-string #\Tab)))
	   (setf notes (split-sequence:split-sequence #\Tab notes :remove-empty-subseqs t))
	   (setf notes (mapcar (lambda (note) (string-left-trim (list #\Newline) note)) notes))
	   (remove "" notes :test #'equal)))
  (call-next-method quotes-list movie stream
		    :extract-notes-fn #'extract-notes-fn :include-empty-lines t)))
(defmethod summary ((quotes quotes) &key)
  (call-next-method quotes :suffix (char-to-string #\Newline)))