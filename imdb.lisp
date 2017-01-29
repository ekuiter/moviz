;;; Helpers

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

;;; Classes

(defclass actors ()
  ((file-name :initarg :file-name
	      :initform (error "Must supply file name")
	      :reader file-name)
   (file-length :initform nil)
   (data-start :initform nil)
   (data-end :initform nil)))

;;; Methods

(defmacro define-lazy-slot (name slot documentation &body body)
  `(defmethod ,name ((actors actors))
     ,documentation
     (with-slots (file-name ,slot) actors
       (or ,slot (setf ,slot (with-open-file (stream file-name) ,@body))))))

(defmacro do-lines (stream var-init-steps end-test &body body)
  `(do ((line (read-line ,stream nil) (read-line ,stream nil)) ,@var-init-steps)
       (,end-test)
     ,@body))

;;; @TODO: own package for file-length, search
(define-lazy-slot get-file-length file-length
    "Returns the file length."
  (file-length stream))

(define-lazy-slot data-start data-start
    "Returns the offset of the first record."
  (do-lines stream () (null line)
    (when (or (equal line "THE ACTRESSES LIST")
	      (equal line "THE ACTORS LIST"))
      (dotimes (i 4)
	(read-line stream nil))
      (return (file-position stream)))))

(define-lazy-slot data-end data-end
    "Returns the offset after the last record."
  (file-position stream (- (get-file-length actors) 100000))
  (do-lines stream () (null line)
    (when (equal line "SUBMITTING UPDATES")
      (dotimes (i 3)
	(back-line stream))
      (return (file-position stream)))))

(defun actor-line-p (line)
  "Returns whether a given line is the start of a record."
  (and (> (length line) 0) (char/= (aref line 0) #\Tab)))

(defmethod end-of-data-p ((actors actors) pos)
  "Returns whether the given offset is after the last record."
  (> pos (data-end actors)))

(defmethod read-until-record ((actors actors) stream)
  "Advances the stream to the next record and returns the next record's actor."
  (back-char stream)
  (unless (char= (read-char stream) #\Newline)
    (read-line stream nil))
  (let ((begin-of-line (file-position stream)))
    (do-lines stream () (null line)
      (when (actor-line-p line)
	(file-position stream begin-of-line)
	(return-from read-until-record
	  (if (end-of-data-p actors begin-of-line)
	      nil
	      (subseq line 0 (position #\Tab line)))))
      (setf begin-of-line (file-position stream)))))

(defmethod read-record ((actors actors) stream)
  "Advances the stream to the next record and returns the current record."
  (when (end-of-data-p actors (file-position stream))
    (return-from read-record nil))
  (let ((record ""))
    (do-lines stream ((i 0 (1+ i))) nil
      (when (and (> i 0) (actor-line-p line))
	(back-line stream)
	(return-from read-record record))
      (setf record (concatenate 'string record line)))))

(defmethod do-search ((actors actors) actor)
  "Returns the record for a specified actor."
  (with-open-file (stream (file-name actors))
    (labels ((binary-search (min max)
	       (when (> min max)
		 (return-from binary-search nil))
	       (let ((mid (floor (+ min max) 2)))
		 (file-position stream mid)
		 (let* ((current-actor (read-until-record actors stream))
			(less (string<= actor current-actor))
			(new-min (if less min mid))
			(new-max (if less mid max)))
		   (when (and (= min new-min) (= max new-max))
		     (return-from binary-search nil))
		   (when (equal actor current-actor)
		     (return-from binary-search (read-record actors stream)))
		   (file-position stream new-min)
		   (binary-search new-min new-max)))))
      (file-position stream (data-start actors))
      (binary-search (data-start actors) (data-end actors)))))

(defmethod report-progress ((actors actors) pos i)
  "Outputs information about the search progress."
  (when (= (mod i 1000000) 0)
    (let* ((div (* 1024 1024))
	  (now (floor pos div))
	  (total (floor (- (data-end actors) (data-start actors)) div)))
      (format t "Searching ~a ... ~3d MB / ~3d MB~%" (file-name actors) now total))))

(defmethod inverse-search ((actors actors) movie)
  "Returns actors matching a specified movie."
  (let ((results nil))
    (with-open-file (stream (file-name actors))
      (file-position stream (data-start actors))
      (let ((current-actor "") (current-entry ""))
	(do-lines stream ((i 0 (1+ i))) (end-of-data-p actors (file-position stream))
	  (report-progress actors (file-position stream) i)
	  (when (> (length line) 0)
	    (setf current-entry line)
	    (when (actor-line-p line)
	      (let ((tab-pos (position #\Tab line)))
		(setf current-actor (subseq line 0 tab-pos))
		(setf current-entry (subseq line tab-pos))))
	    (setf current-entry (string-left-trim (list #\Tab) current-entry))
	    (when (search movie current-entry)
	      (push current-actor results))))))
    results))

(defvar *actors* (make-instance 'actors :file-name "~/graph/imdb/actors.list"))
(defvar *actresses* (make-instance 'actors :file-name "~/graph/imdb/actresses.list"))