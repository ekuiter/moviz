(in-package :imdb)

(defclass movie-information ()
  ((movie :initarg :movie
	  :initform (error "Must supply movie")
	  :reader movie)
   (episode :initarg :episode
	    :initform nil
	    :reader episode)
   (notes :initarg :notes
	  :initform (error "Must supply notes")
	  :reader notes)))

(defclass movie-information-list (imdb-list) ())

(define-lazy-slot movie-information-list data-end
    "Returns the offset after the last record."
    (file-length instance))

(defgeneric movie-information-class (movie-information-list)
  (:documentation "Returns the associated movie information class."))

(defmethod print-object ((info movie-information) stream)
  "Prints movie information."
  (print-unreadable-object (info stream :type t)
    (format stream "~a~@[ - ~a~]"
	    (title (movie info)) (episode info))))

(defmethod summary (object)
  "Returns a summary of an object."
  (format nil "~a" object))

(defmethod summary ((info movie-information))
  "Returns movie information in a readable form."
  (let ((heading (format nil "~a~@[ - ~a~]" (title (movie info)) (episode info))))
    (format nil "~a~%~v@{=~}~%~{~a~%~}" heading (length heading) (notes info))))

(defun summarize (object)
  "Prints a summary of an object."
  (format t "~a~%" (summary object)))

(defun summarize-all (list)
  "Prints a summary of a list."
  (loop for element in list do (summarize element)))

(defmethod record-line-p ((info-list movie-information-list) line)
  (and (> (length line) 0) (char= (aref line 0) #\#)))

(defun extract-movie-information-id (line)
  "Extracts the movie title from a record's first line."
  (let ((results (cl-ppcre:register-groups-bind (title episode)
		     ("# (.*?) \\([\\d?]{4}(?:.*\\{(.*?)\\})?" line)
		   (list title episode))))
    (if results (apply #'values results) (subseq line 2))))

(defmethod read-until-record ((info-list movie-information-list) stream &key)
  (call-next-method info-list stream :extract-id-fn #'extract-movie-information-id))

(defmethod read-record ((info-list movie-information-list) movie stream &key)
  (flet ((extract-record-fn (record)
	   (let* ((new-line-pos (position #\Newline record))
		  (notes (subseq record new-line-pos)))
	     (setf notes (cl-ppcre:regex-replace-all "\\n " notes ""))
	     (setf notes (split-sequence:split-sequence #\Newline notes :remove-empty-subseqs t))
	     (setf notes (mapcar (lambda (note) (subseq note 2)) notes))
	     (multiple-value-bind (title episode)
		 (extract-movie-information-id (subseq record 0 new-line-pos))
	       (declare (ignore title))
	       (make-instance (movie-information-class info-list)
			      :movie movie :episode episode :notes notes)))))
    (call-next-method info-list movie stream :extract-record-fn #'extract-record-fn)))

(defmethod do-search ((info-list movie-information-list) (movie movie))
  (do-search-linear info-list movie :id (title movie)
		    :id= (lambda (id current-id) (equal id (string-trim "\"" current-id)))))