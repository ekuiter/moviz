(defvar *file-name* "~/graph/imdb/actresses.list")
(defvar *file-length* nil)
(defvar *file-start* nil)
(defvar *file-end* nil)

(defun back-char (stream)
  (let ((pos (file-position stream)))
    (when (> pos 0)
      (file-position stream (- pos 1))
      (peek-char nil stream))))

(defun back-line (stream)
  (dotimes (i 2)
    (do ((ch (back-char stream) (back-char stream)))
	((char= ch #\Newline))))
  (read-char stream nil))

(defun get-file-length ()
  (or *file-length*
      (setf *file-length*
	    (with-open-file (in *file-name*)
	      (file-length in)))))

(defun get-file-start ()
  (or *file-start*
      (setf *file-start*
	    (with-open-file (stream *file-name*)
	      (do ((line (read-line stream nil) (read-line stream nil)))
		  ((null line))
		(when (equal line "THE ACTRESSES LIST")
		  (dotimes (i 4)
		    (read-line stream nil))
		  (return (file-position stream))))))))

(defun get-file-end ()
  (or *file-end*
      (setf *file-end*
	    (with-open-file (stream *file-name*)
	      (file-position stream (- (get-file-length) 100000))
	      (do ((line (read-line stream nil) (read-line stream nil)))
		  ((null line))
		(when (equal line "SUBMITTING UPDATES")
		  (dotimes (i 3)
		    (back-line stream))
		  (return (file-position stream))))))))

(defun read-until-record (stream)
  (back-char stream)
  (unless (char= (read-char stream) #\Newline)
    (read-line stream nil))
  (let ((begin-of-line (file-position stream)))
    (do ((line (read-line stream nil) (read-line stream nil)))
	((null line))
      (when (and (> (length line) 0) (char/= (aref line 0) #\Tab))
	(file-position stream begin-of-line)
	(return-from read-until-record
	  (if (<= begin-of-line (get-file-end))
	      (subseq line 0 (position #\Tab line))
	      nil)))
      (setf begin-of-line (file-position stream)))))

(defun read-record (stream)
  (let ((record ""))
    (do ((line (read-line stream nil) (read-line stream nil))
	 (i 0 (1+ i)))
	(nil)
      (when (and (> i 0) (> (length line) 0) (char/= (aref line 0) #\Tab))
	(back-line stream)
	(return-from read-record record))
      (setf record (concatenate 'string record line)))))

(defun search-actor (actor)
  (with-open-file (stream *file-name*)
    (labels ((binary-search (min max)
	       (format t "Doing binary search between ~a and ~a~%" min max)
	       (when (> min max)
		 (return-from binary-search nil))
	       (let ((mid (floor (+ min max) 2)))
		 (file-position stream mid)
		 (let* ((current-actor (read-until-record stream))
			(less (string<= actor current-actor))
			(new-min (if less min mid))
			(new-max (if less mid max)))
		   (when (and (= min new-min) (= max new-max))
		     (return-from binary-search nil))
		   (when (equal actor current-actor)
		     (return-from binary-search (read-record stream)))
		   (file-position stream new-min)
		   (binary-search new-min new-max)))))
      (file-position stream (get-file-start))
      (binary-search (get-file-start) (get-file-end)))))