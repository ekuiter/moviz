(in-package :imdb)

(defparameter +imdb-path+ "imdb/")

(defclass movie ()
  ((title :initarg :title
	  :initarg :string
	  :initform (error "Must supply title")
	  :reader title)))

(defclass movie-record ()
  ((movie :initarg :movie
	  :initform (error "Must supply movie")
	  :reader movie)
   (episode :initarg :episode
	    :initform nil
	    :reader episode)
   (info :initarg :info
	 :initform (error "Must supply info")
	 :reader info)))

(defclass imdb-list ()
  ((file-name :initarg :file-name
	      :initform (error "Must supply file name")
	      :reader file-name)
   (file-length :initform nil)
   (data-start :initform nil)
   (data-end :initform nil)
   (notice-shown :initform nil
		 :allocation :class)))

(defun back-char (stream)
  "Undo a read-char operation."
  (let ((pos (file-position stream)))
    (when (> pos 0)
      (file-position stream (- pos 1))
      (peek-char nil stream))))

(defun back-line (stream)
  "Undo a read-line operation (jumps to begin of line)."
  (dotimes (i 2)
    (do ((ch (back-char stream) (back-char stream)))
	((char= ch #\Newline))))
  (read-char stream nil))

(defmacro with-open-list (file-name &body body)
  "Creates a stream for a list file."
  `(with-open-file (stream (file-name ,file-name) :external-format :iso-8859-1)
     (declare (ignorable stream))
     ,@body))

(defmacro define-lazy-slot (class slot documentation &body body)
  "Defines an accessor that only computes its value only once."
  `(defmethod ,slot ((instance ,class))
     ,documentation
     (with-slots (file-name ,slot) instance
       (or ,slot (setf ,slot (with-open-list instance ,@body))))))

(defmacro do-lines (stream var-init-steps end-test &body body)
  "Iterates over a file line by line."
  `(do ((line (read-line ,stream nil) (read-line ,stream nil)) ,@var-init-steps)
       (,end-test)
     ,@body))

(defmethod print-object ((movie movie) stream)
  "Prints a movie."
  (print-unreadable-object (movie stream :type t)
    (format stream "~a" (title movie))))

(defmethod movie= ((movie-1 movie) (movie-2 movie))
  "Tests whether two movies are equal."
  (equal (title movie-1) (title movie-2)))

(defun make-list-instance (class &optional file-name)
  "Creates a list file."
  (make-instance (intern (format nil "~:@(~a~)-LIST" class) :imdb)
		  :file-name (format nil "~a~(~a~).list" +imdb-path+
				      (if file-name file-name class))))

(defmethod initialize-instance :after ((list imdb-list) &key)
  "Initializes a list file."
  (unless (probe-file (file-name list))
    (error "list file not found. check (ccl:current-directory)")))

(defmethod show-notice ((list imdb-list))
  (unless (slot-value list 'notice-shown)
    (format t "Information courtesy of IMDb (http://www.imdb.com). Used with permission.~%")
    (setf (slot-value list 'notice-shown) t)))

(defgeneric id-class (imdb-list)
  (:documentation "Returns the associated id class."))

(defgeneric inverse-id-class (imdb-list)
  (:documentation "Returns the associated id class for inverse searching."))

(defgeneric record-class (imdb-list)
  (:documentation "Returns the associated record class."))

(define-lazy-slot imdb-list file-length
    "Returns the file length."
  (cl:file-length stream))

(defmacro define-data-bound-slot (class bound bounding-lines additional-lines)
  (let ((startp (eql bound 'start)))
    `(define-lazy-slot ,class ,(if startp 'data-start 'data-end)
	 ,(if startp "Returns the offset of the first record."
	      "Returns the offset after the last record.")
       ,(unless startp `(file-position stream (- (file-length instance) 100000)))
       (do-lines stream () (null line)
	 (when (or ,@(mapcar (lambda (bounding-line) `(equal line ,bounding-line))
			     (if (listp bounding-lines) bounding-lines (list bounding-lines))))
	   (dotimes (i ,additional-lines)
	     ,(if startp `(read-line stream nil) `(back-line stream)))
	   (return (file-position stream)))))))

(defgeneric data-start (list)
  (:documentation "Returns the offset of the first record."))

(defgeneric data-end (list)
  (:documentation "Returns the offset after the last record."))

(defmethod data-length ((list imdb-list))
  "Returns the data length."
  (- (data-end list) (data-start list)))

(defmethod end-of-data-p ((list imdb-list) pos)
  "Returns whether the given offset is after the last record."
  (>= pos (data-end list)))

(defmethod summary (object &key)
  "Returns a summary of an object."
  (format nil "~a" object))

(defmethod summarize (object)
  "Prints a summary of an object."
  (format t "~a~%" (summary object)))

(defmethod summarize ((list cons))
  "Prints a summary of a list."
  (loop for element in list do (summarize element)))

(defmethod print-object ((info movie-record) stream)
  "Prints a movie record."
  (print-unreadable-object (info stream :type t)
    (format stream "~a~@[ - ~a~]"
	    (title (movie info)) (episode info))))

(defmethod summary ((info movie-record) &key (suffix ""))
  "Returns movie record in a readable form."
  (let ((heading (format nil "~a~@[ - ~a~]" (title (movie info)) (episode info))))
    (format nil "~a~%~v@{=~}~%~{~a~%~}" heading (length heading)
	    (mapcar (lambda (note) (format nil "~a~a" note suffix)) (info info)))))

(defun delete-duplicates-in-sorted-list (list &key (test #'eql))
  "Deletes all duplicates in a sorted list (more efficient than delete-duplicates)."
  (mapcon (lambda (cell)
	    (when (or (null (cdr cell)) (not (funcall test (car cell) (cadr cell))))
	      (list (car cell))))
	  list))

(defmethod check-record ((list imdb-list) id-object bound &optional (offset 1000))
  "Returns the first or last record in a list, useful for debugging."
  (with-open-list list
    (file-position stream (if (eql bound :start) (data-start list) (- (data-end list) offset)))
    (values (read-until-record list stream) (read-record list id-object stream))))

(defgeneric record-line-p (list line)
  (:documentation "Returns whether a given line is the start of or after the last record."))

(defmethod read-until-record ((list imdb-list) stream &key extract-id-fn)
  "Advances the stream to the next record and returns the next record's id."
  (back-char stream)
  (unless (char= (read-char stream) #\Newline)
    (read-line stream nil))
  (let ((begin-of-line (file-position stream)))
    (do-lines stream () (null line)
      (when (record-line-p list line)
	(file-position stream begin-of-line)
	(return-from read-until-record
	  (unless (end-of-data-p list begin-of-line)
	    (funcall extract-id-fn line))))
      (setf begin-of-line (file-position stream)))))

(defmethod read-record ((list imdb-list) id-object stream
			&key extract-record-fn include-empty-lines)
  "Advances the stream to the next record and returns the current record."
  (declare (ignore id-object))
  (when (end-of-data-p list (file-position stream))
    (return-from read-record nil))
  (let ((record ""))
    (do-lines stream ((i 0 (1+ i))) (null line)
      (when (and (> i 0) (record-line-p list line))
	(back-line stream)
	(return))
      (when (or include-empty-lines (> (length line) 0))
	(setf record (concatenate 'string record line (list #\Newline)))))
    (funcall extract-record-fn record)))

(defmethod do-search-linear ((list imdb-list) id-object &key id (id= #'equal))
  "Returns all records for a specified id object using linear search."
  (show-notice list)
  (with-open-list list
    (file-position stream (data-start list))
    (loop for current-id = (read-until-record list stream) while current-id
	 if (funcall id= id current-id) collect (read-record list id-object stream)
	 else do (read-char stream))))

(defmethod do-search-binary ((list imdb-list) id-object &key id (id= #'equal) (id<= #'string<=))
  "Returns the record for a specified id object using binary search."
  (show-notice list)
  (with-open-list list
    (labels ((binary-search (min max)
	       (when (> min max)
		 (return-from binary-search nil))
	       (let ((mid (floor (+ min max) 2)))
		 (file-position stream mid)
		 (let* ((current-id (read-until-record list stream))
			(pos (file-position stream))
			(less (funcall id<= id current-id))
			(new-min (if less min mid))
			(new-max (if less mid max)))
		   (when (and (= min new-min) (= max new-max))
		     (return-from binary-search nil))
		   (when (funcall id= id current-id)
		     (return-from binary-search
		       (values (read-record list id-object stream) pos)))
		   (file-position stream new-min)
		   (binary-search new-min new-max)))))
      (file-position stream (data-start list))
      (binary-search (data-start list) (data-end list)))))
