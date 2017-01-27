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

;;; @TODO: own package for file-length, search
(defmethod get-file-length ((actors actors))
  "Returns the file length."
  (with-slots (file-name file-length) actors
    (or file-length
	(setf file-length
	      (with-open-file (stream file-name)
		(file-length stream))))))

(defmethod data-start ((actors actors))
  "Returns the offset of the first record."
  (with-slots (file-name data-start) actors
    (or data-start
	(setf data-start
	      (with-open-file (stream file-name)
		(do ((line (read-line stream nil) (read-line stream nil)))
		    ((null line))
		  (when (or (equal line "THE ACTRESSES LIST")
			    (equal line "THE ACTORS LIST"))
		    (dotimes (i 4)
		      (read-line stream nil))
		    (return (file-position stream)))))))))

(defmethod data-end ((actors actors))
  "Returns the offset after the last record."
  (with-slots (file-name data-end) actors
    (or data-end
	(setf data-end
	      (with-open-file (stream file-name)
		(file-position stream (- (get-file-length actors) 100000))
		(do ((line (read-line stream nil) (read-line stream nil)))
		    ((null line))
		  (when (equal line "SUBMITTING UPDATES")
		    (dotimes (i 3)
		      (back-line stream))
		    (return (file-position stream)))))))))

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

(defvar *actors* (make-instance 'actors :file-name "~/graph/imdb/actors.list"))
(defvar *actresses* (make-instance 'actors :file-name "~/graph/imdb/actresses.list"))