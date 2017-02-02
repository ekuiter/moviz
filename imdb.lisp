(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :split-sequence)
  (ql:quickload :cl-ppcre))

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

(defmacro with-open-actors-file (actors &body body)
  "Creates a stream for an actors file."
  `(with-open-file (stream (file-name ,actors) :external-format :iso-8859-1) ,@body))

(defmacro define-lazy-slot (name slot documentation &body body)
  "Defines an accessor that only computes its value only once."
  `(defmethod ,name ((actors actors))
     ,documentation
     (with-slots (file-name ,slot) actors
       (or ,slot (setf ,slot (with-open-actors-file actors ,@body))))))

(defmacro do-lines (stream var-init-steps end-test &body body)
  "Iterates over a file line by line."
  `(do ((line (read-line ,stream nil) (read-line ,stream nil)) ,@var-init-steps)
       (,end-test)
     ,@body))

;;; Classes

(defclass actor ()
  ((first-name :initarg :first-name
	       :initform nil
	       :reader first-name)
   (last-name :initarg :last-name
	      :initform nil
	      :reader last-name)
   (number :initarg :number
	   :initform nil
	   :reader number)))

(defclass raw-actor (actor) ())

(defclass movie ()
  ((title :initarg :title
	  :initform nil
	  :reader title)))

(defclass actors ()
  ((file-name :initarg :file-name
	      :initform (error "Must supply file name")
	      :reader file-name)
   (file-length :initform nil)
   (data-start :initform nil)
   (data-end :initform nil)))

;;; Methods

(defmethod initialize-instance :after ((actor actor) &key name raw-name)
  "Initializes an actor."
  (when name
    (destructuring-bind (last-name first-name) (split-sequence:split-sequence #\, name)
      (let ((paren-pos (position #\( first-name)))
	(setf (slot-value actor 'first-name)
	      (string-trim " " (subseq first-name 0 paren-pos)))
	(setf (slot-value actor 'last-name) last-name)
	(when paren-pos
	  (setf (slot-value actor 'number)
		(subseq first-name (1+ paren-pos) (position #\) first-name)))))))
  (unless raw-name
    (unless (and (first-name actor) (last-name actor))
      (error "Must supply name"))))

(defmethod initialize-instance :after ((actor raw-actor) &key raw-name)
  "Initializes an actor with a raw name."
  (unless raw-name
    (error "Must supply name"))
  (setf (slot-value actor 'first-name) "")
  (setf (slot-value actor 'last-name) raw-name))

(defmethod print-object ((actor actor) stream)
  "Prints an actor."
  (print-unreadable-object (actor stream :type t)
    (format stream "~a" (name actor))))

(defmethod name ((actor actor))
  "Returns an actor's name as used in an actors file."
  (format nil "~a, ~a~@[ (~a)~]" (last-name actor) (first-name actor) (number actor)))

(defmethod name ((actor raw-actor))
  "Returns a raw actor's name."
  (last-name actor))

(defun line-to-movie-title (line)
  "Extracts a movie's title from an actor file line."
  (cl-ppcre:register-groups-bind (title) ("\"?(.*?)\"? \\([\\d?]{4}" line) title))

(defmethod initialize-instance :after ((movie movie) &key line)
  "Initiales a movie."
  (when line
    (setf (slot-value movie 'title) (line-to-movie-title line)))
  (unless (title movie)
    (error "Must supply title")))

(defmethod print-object ((movie movie) stream)
  "Prints a movie."
  (print-unreadable-object (movie stream :type t)
    (format stream "~a" (title movie))))

(defmethod movie= ((movie-1 movie) (movie-2 movie))
  "Tests whether two movies are equal."
  (equal (title movie-1) (title movie-2)))

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

(defmethod data-length ((actors actors))
  "Returns the data length."
  (- (data-end actors) (data-start actors)))

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
	(setf record (subseq record (position #\Tab record)))
	(return))
      (setf record (concatenate 'string record line)))
    (setf record (mapcar (lambda (line) (make-instance 'movie :line line))
			 (split-sequence:split-sequence #\Tab record :remove-empty-subseqs t)))
    (mapcon (lambda (cell)
	       (when (or (null (cdr cell)) (not (movie= (car cell) (cadr cell))))
		 (list (car cell)))) record)))

(defmethod do-search ((actors actors) (actor actor))
  "Returns the record for a specified actor."
  (with-open-actors-file actors
    (let ((actor-name (name actor)))
      (labels ((binary-search (min max)
		 (when (> min max)
		   (return-from binary-search nil))
		 (let ((mid (floor (+ min max) 2)))
		   (file-position stream mid)
		   (let* ((current-actor (read-until-record actors stream))
			  (less (string<= actor-name current-actor))
			  (new-min (if less min mid))
			  (new-max (if less mid max)))
		     (when (and (= min new-min) (= max new-max))
		       (return-from binary-search nil))
		     (when (equal actor-name current-actor)
		       (return-from binary-search (read-record actors stream)))
		     (file-position stream new-min)
		     (binary-search new-min new-max)))))
	(file-position stream (data-start actors))
	(binary-search (data-start actors) (data-end actors))))))

(defun file-size-string (bytes)
  "Returns a human-readable file size."
  (format nil "~3d MB" (floor bytes (* 1024 1024))))

(defmethod inverse-search-partition
    ((actors actors) (movies cons) n i progress progress-changed)
  "Returns actors in a partition matching the specified movies."
  (when (< n 1) (error "at least one partition needed"))
  (when (or (< i 0) (>= i n)) (error "illegal partition index"))
  (let* ((results (make-hash-table :test 'equal))
	 (partition-step (floor (data-length actors) n))
	 (partition-start (+ (data-start actors) (* partition-step i)))
	 (partition-end (if (= i (- n 1))
			    (data-end actors)
			    (+ (data-start actors) (* partition-step (1+ i))))))
    (with-open-actors-file actors
      (file-position stream partition-start)
      (read-until-record actors stream)
      (let ((current-actor "") (current-entry ""))
	(do-lines stream ((lines 0 (1+ lines))) (end-of-data-p actors (file-position stream))
	  (when (= (mod lines 500000) 0)
	    (setf (elt progress i) (- (file-position stream) partition-start))
	    (ccl:signal-semaphore progress-changed))
	  (when (and (> (file-position stream) partition-end) (actor-line-p line))
	    (return))
	  (when (> (length line) 0)
	    (setf current-entry line)
	    (when (actor-line-p line)
	      (let ((tab-pos (position #\Tab line)))
		(setf current-actor (subseq line 0 tab-pos))
		(setf current-entry (subseq line tab-pos))))
	    (setf current-entry (string-left-trim (list #\Tab) current-entry))
	    (loop for movie in movies do
		 (when (eql (search (title movie) (string-left-trim "\"" current-entry)) 0)
		   (push (make-instance 'actor :name current-actor)
			 (gethash movie results))))))))
    (setf (elt progress i) -1)
    (ccl:signal-semaphore progress-changed)
    results))

(defmethod inverse-search ((actors actors) movie &optional (n 4))
  "Returns actors matching a specified movie."
  (gethash movie (inverse-search actors (list movie) n)))

(defmethod inverse-search ((actors actors) (movies cons) &optional (n 4))
  "Returns actors matching the specified movies."
  (labels ((fn (i progress progress-changed)
	     (inverse-search-partition actors movies n i progress progress-changed)))
    (let* ((results (make-hash-table :test 'eq))
	   (progress (make-array n :initial-element 0))
	   (progress-changed (ccl:make-semaphore))
	   (processes
	    (loop for i from 0 to (- n 1) collect
		 (ccl:process-run-function "inverse-search" #'fn i progress progress-changed))))
      (loop for i = 0 then (1+ i)
	 while (>= (loop for i across progress minimize i) 0) do
	   (when (= (mod i n) (- n 1))
	     (format t "~2d% " (floor (* (loop for i across progress sum i) 100)
				      (data-length actors))))
	   (ccl:wait-on-semaphore progress-changed))
      (loop for process in processes do
	   (let ((process-results (ccl:join-process process)))
	     (loop for movie in movies do
		  (setf (gethash movie results)
			(append (gethash movie results) (gethash movie process-results))))))
      results)))

(defvar *actors* (make-instance 'actors :file-name "~/graph/imdb/actors.list"))
(defvar *actresses* (make-instance 'actors :file-name "~/graph/imdb/actresses.list"))