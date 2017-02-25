(in-package :imdb)

(defclass alternate-version ()
  ((movie :initarg :movie
	  :initform (error "Must supply movie")
	  :reader movie)
   (episode :initarg :episode
	    :initform nil
	    :reader episode)
   (notes :initarg :notes
	  :initform (error "Must supply notes")
	  :reader notes)))

(defclass alternate-versions-list (imdb-list) ())

(define-data-bound-slot alternate-versions-list start "ALTERNATE VERSIONS LIST" 3)
(define-data-bound-slot alternate-versions-list end "SUBMITTING NEW DATA" 11)

(defmethod print-object ((av alternate-version) stream)
  "Prints an alternate version."
  (print-unreadable-object (av stream :type t)
    (format stream "~a~@[ - ~a~]"
	    (title (movie av)) (episode av))))

(defmethod summary ((av alternate-version))
  "Returns an alternate version in a readable form."
  (format nil "Alternate version for ~a~@[ - ~a~]:~%~{~a~%~}"
	  (title (movie av)) (episode av) (notes av)))

(defmethod record-line-p ((av-list alternate-versions-list) line)
  (and (> (length line) 0) (or (char= (aref line 0) #\#)
			       (equal (subseq line 0 2) "--"))))

(defun extract-alternate-version-id (line)
  "Extracts the movie title from a record's first line."
  (let ((results (cl-ppcre:register-groups-bind (title episode)
		     ("# (.*?) \\([\\d?]{4}(?:.*\\{(.*?)\\})?" line)
		   (list title episode))))
    (if results (apply #'values results) (subseq line 2))))

(defmethod read-until-record ((av-list alternate-versions-list) stream &key)
  (call-next-method av-list stream :extract-id-fn #'extract-alternate-version-id))

(defmethod read-record ((av-list alternate-versions-list) movie stream &key)
  (flet ((extract-record-fn (record)
	   (let* ((new-line-pos (position #\Newline record))
		  (notes (subseq record new-line-pos)))
	     (setf notes (cl-ppcre:regex-replace-all "\\n " notes ""))
	     (setf notes (split-sequence:split-sequence #\Newline notes :remove-empty-subseqs t))
	     (setf notes (mapcar (lambda (note) (subseq note 2)) notes))
	     (multiple-value-bind (title episode)
		 (extract-alternate-version-id (subseq record 0 new-line-pos))
	       (declare (ignore title))
	       (make-instance 'alternate-version :movie movie :episode episode :notes notes)))))
    (call-next-method av-list movie stream :extract-record-fn #'extract-record-fn)))

(defmethod do-search ((av-list alternate-versions-list) (movie movie))
  (do-search-linear av-list movie :id (title movie)
		    :id= (lambda (id current-id) (equal id (string-trim "\"" current-id)))))