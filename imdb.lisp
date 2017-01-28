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

(defmacro define-file-method (name slot documentation &body body)
  `(defmethod ,name ((actors actors))
     ,documentation
     (with-slots (file-name ,slot) actors
       (or ,slot (setf ,slot (with-open-file (stream file-name) ,@body))))))

;;; @TODO: own package for file-length, search
(define-file-method get-file-length file-length
    "Returns the file length."
  (file-length stream))

(define-file-method data-start data-start
    "Returns the offset of the first record."
  (do ((line (read-line stream nil) (read-line stream nil)))
      ((null line))
    (when (or (equal line "THE ACTRESSES LIST")
	      (equal line "THE ACTORS LIST"))
      (dotimes (i 4)
	(read-line stream nil))
      (return (file-position stream)))))
(define-file-method data-end data-end
    "Returns the offset after the last record."
  (file-position stream (- (get-file-length actors) 100000))
  (do ((line (read-line stream nil) (read-line stream nil)))
      ((null line))
    (when (equal line "SUBMITTING UPDATES")
      (dotimes (i 3)
	(back-line stream))
      (return (file-position stream)))))

(defmethod read-until-record ((actors actors) stream)
  "Advances the stream to the next record and returns the next record's actor."
  (back-char stream)
  (unless (char= (read-char stream) #\Newline)
    (read-line stream nil))
  (let ((begin-of-line (file-position stream)))
    (do ((line (read-line stream nil) (read-line stream nil)))
	((null line))
      (when (and (> (length line) 0) (char/= (aref line 0) #\Tab))
	(file-position stream begin-of-line)
	(return-from read-until-record
	  (if (<= begin-of-line (data-end actors))
	      (subseq line 0 (position #\Tab line))
	      nil)))
      (setf begin-of-line (file-position stream)))))

(defmethod read-record ((actors actors) stream)
  "Advances the stream to the next record and returns the current record."
  (when (> (file-position stream) (data-end actors))
    (return-from read-record nil))
  (let ((record ""))
    (do ((line (read-line stream nil) (read-line stream nil))
	 (i 0 (1+ i)))
	(nil)
      (when (and (> i 0) (> (length line) 0) (char/= (aref line 0) #\Tab))
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

(defmethod inverse-search ((actors actors) movie)
  "Returns actors matching a specified movie."
  (let ((results nil))
    (with-open-file (stream (file-name actors))
      (file-position stream (data-start actors))
      (let ((current-actor "") (current-entry ""))
	(do ((line (read-line stream nil) (read-line stream nil))
	     (i 0 (1+ i)))
	    ((> (file-position stream) (data-end actors)))
	  (when (= (mod i 1000000) 0)
	    (format t "~a~%" (floor (floor (file-position stream) 1024) 1024)))
	  (when (> (length line) 0)
	    (setf current-entry line)
	    (when (char/= (aref line 0) #\Tab)
	      (let ((tab-pos (position #\Tab line)))
		(setf current-actor (subseq line 0 tab-pos))
		(setf current-entry (subseq line tab-pos))))
	    (setf current-entry (string-left-trim (list #\Tab) current-entry))
	    (when (search movie current-entry)
	      (push current-actor results))))))
    results))

(defvar *actors* (make-instance 'actors :file-name "~/graph/imdb/actors.list"))
(defvar *actresses* (make-instance 'actors :file-name "~/graph/imdb/actresses.list"))