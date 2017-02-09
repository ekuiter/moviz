(in-package :actor-list)

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

(defmacro with-open-actor-list (actor-list &body body)
  "Creates a stream for an actor list."
  `(with-open-file (stream (file-name ,actor-list) :external-format :iso-8859-1) ,@body))

(defmacro define-lazy-slot (slot documentation &body body)
  "Defines an accessor that only computes its value only once."
  `(defmethod ,slot ((actor-list actor-list))
     ,documentation
     (with-slots (file-name ,slot) actor-list
       (or ,slot (setf ,slot (with-open-actor-list actor-list ,@body))))))

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

(defclass movie ()
  ((title :initarg :title
	  :initform (error "Must supply title")
	  :reader title)))

(defclass role ()
  ((actor :initarg :actor
	  :initform (error "Must supply actor")
	  :reader actor)
   (movie :initarg :movie
	  :initform nil
	  :reader movie)
   (name :initarg :name
		   :initform nil
		   :reader name)
   (billing :initarg :billing
	    :initform nil
	    :reader billing)))

(defclass actor-list ()
  ((file-name :initarg :file-name
	      :initform (error "Must supply file name")
	      :reader file-name)
   (file-length :initform nil)
   (data-start :initform nil)
   (data-end :initform nil)
   (notice-shown :initform nil
		 :allocation :class)))

;;; Methods

(defmethod initialize-instance :after ((actor actor) &key name)
  "Initializes an actor."
  (when name
    (destructuring-bind (last-name first-name)
	(if (find #\, name) (split-sequence:split-sequence #\, name) (list "" name))
      (let ((paren-pos (position #\( first-name)))
	(setf (slot-value actor 'first-name)
	      (string-trim " " (subseq first-name 0 paren-pos)))
	(setf (slot-value actor 'last-name) last-name)
	(when paren-pos
	  (setf (slot-value actor 'number)
		(subseq first-name (1+ paren-pos) (position #\) first-name)))))))
  (unless (and (first-name actor) (last-name actor))
    (error "Must supply name")))

(defmethod print-object ((actor actor) stream)
  "Prints an actor."
  (print-unreadable-object (actor stream :type t)
    (format stream "~a" (name actor))))

(defmethod name ((actor actor))
  "Returns an actor's name as used in an actor list."
  (let ((last-name (unless (equal (last-name actor) "") (last-name actor))))
    (format nil "~@[~a, ~]~a~@[ (~a)~]" last-name (first-name actor) (number actor))))

(defmethod actor= ((actor-1 actor) (actor-2 actor))
  "Tests whether two actors are equal."
  (and (equal (first-name actor-1) (first-name actor-2))
       (equal (last-name actor-1) (last-name actor-2))
       (equal (number actor-1) (number actor-2))))

(defmethod actor< ((actor-1 actor) (actor-2 actor))
  "Tests whether an actor is less than another."
  (string-lessp (name actor-1) (name actor-2)))

(defmethod print-object ((movie movie) stream)
  "Prints a movie."
  (print-unreadable-object (movie stream :type t)
    (format stream "~a" (title movie))))

(defmethod movie= ((movie-1 movie) (movie-2 movie))
  "Tests whether two movies are equal."
  (equal (title movie-1) (title movie-2)))

(define-condition skip-role (error) ())

(defun line-to-parts (line)
  "Extracts a role's parts from an actor file line."
  (cl-ppcre:register-groups-bind (title voice name billing)
      ("\"?(.*?)\"? \\([\\d?]{4}(?:.*(\\(voice\\))|(?:.*\\[(.*?)\\])?(?:.*<(.*?)>)?)" line)
    (when voice
      (error 'skip-role))
    (values title name billing)))

(defmethod initialize-instance :after ((role role) &key line)
   "Initializes a role."
 (when line
   (multiple-value-bind (title name billing) (line-to-parts line)
     (unless title
       (format t "Problem with line: ~a" line))
     (setf (slot-value role 'movie) (make-instance 'movie :title title))
     (setf (slot-value role 'name) name)
     (setf (slot-value role 'billing) billing)))
  (unless (movie role)
    (error "Must supply movie")))

(defmethod print-object ((role role) stream)
  "Prints a role."
  (print-unreadable-object (role stream :type t)
    (with-slots (actor movie name) role
    (format stream "~a in ~a~@[ as ~a~]" (name actor) (title movie) name))))

(defmethod role= ((role-1 role) (role-2 role))
  "Tests whether two roles are equal."
  (and (actor= (actor role-1) (actor role-2))
       (movie= (movie role-1) (movie role-2))))

(defmethod initialize-instance :after ((actor-list actor-list) &key)
  "Initializes an actor list."
  (unless (probe-file (file-name actor-list))
    (error "actor list file not found. check (ccl:current-directory)")))

(defmethod show-notice ((actor-list actor-list))
  (unless (slot-value actor-list 'notice-shown)
    (format t "Information courtesy of IMDb (http://www.imdb.com). Used with permission.~%")
    (setf (slot-value actor-list 'notice-shown) t)))

(define-lazy-slot file-length
    "Returns the file length."
  (cl:file-length stream))

(define-lazy-slot data-start
    "Returns the offset of the first record."
  (do-lines stream () (null line)
    (when (or (equal line "THE ACTRESSES LIST")
	      (equal line "THE ACTORS LIST"))
      (dotimes (i 4)
	(read-line stream nil))
      (return (file-position stream)))))

(define-lazy-slot data-end
    "Returns the offset after the last record."
  (file-position stream (- (file-length actor-list) 100000))
  (do-lines stream () (null line)
    (when (equal line "SUBMITTING UPDATES")
      (dotimes (i 3)
	(back-line stream))
      (return (file-position stream)))))

(defmethod data-length ((actor-list actor-list))
  "Returns the data length."
  (- (data-end actor-list) (data-start actor-list)))

(defun actor-line-p (line)
  "Returns whether a given line is the start of a record."
  (and (> (length line) 0) (char/= (aref line 0) #\Tab)))

(defmethod end-of-data-p ((actor-list actor-list) pos)
  "Returns whether the given offset is after the last record."
  (> pos (data-end actor-list)))

(defmethod read-until-record ((actor-list actor-list) stream)
  "Advances the stream to the next record and returns the next record's actor."
  (back-char stream)
  (unless (char= (read-char stream) #\Newline)
    (read-line stream nil))
  (let ((begin-of-line (file-position stream)))
    (do-lines stream () (null line)
      (when (actor-line-p line)
	(file-position stream begin-of-line)
	(return-from read-until-record
	  (if (end-of-data-p actor-list begin-of-line)
	      nil
	      (subseq line 0 (position #\Tab line)))))
      (setf begin-of-line (file-position stream)))))

(defun delete-duplicates-in-sorted-list (list &key (test #'eql))
  "Deletes all duplicates in a sorted list (more efficient than delete-duplicates)."
  (mapcon (lambda (cell)
	    (when (or (null (cdr cell)) (not (funcall test (car cell) (cadr cell))))
	      (list (car cell)))) list))

(defmethod read-record ((actor-list actor-list) actor stream)
  "Advances the stream to the next record and returns the current record."
  (when (end-of-data-p actor-list (file-position stream))
    (return-from read-record nil))
  (let ((record ""))
    (do-lines stream ((i 0 (1+ i))) nil
      (when (and (> i 0) (actor-line-p line))
	(back-line stream)
	(setf record (subseq record (position #\Tab record)))
	(return))
      (setf record (concatenate 'string record line)))
    (setf record (mapcan (lambda (line) (handler-case
					    (list (make-instance 'role :actor actor :line line))
					  (skip-role ())))
			 (split-sequence:split-sequence #\Tab record :remove-empty-subseqs t)))
    (delete-duplicates-in-sorted-list record :test #'role=)))

(defmethod do-search ((actor-list actor-list) (actor actor))
  "Returns the record for a specified actor."
  (show-notice actor-list)
  (with-open-actor-list actor-list
    (let ((actor-name (name actor)))
      (labels ((binary-search (min max)
		 (when (> min max)
		   (return-from binary-search nil))
		 (let ((mid (floor (+ min max) 2)))
		   (file-position stream mid)
		   (let* ((current-actor (read-until-record actor-list stream))
			  (less (string<= actor-name current-actor))
			  (new-min (if less min mid))
			  (new-max (if less mid max)))
		     (when (and (= min new-min) (= max new-max))
		       (return-from binary-search nil))
		     (when (equal actor-name current-actor)
		       (return-from binary-search (read-record actor-list actor stream)))
		     (file-position stream new-min)
		     (binary-search new-min new-max)))))
	(file-position stream (data-start actor-list))
	(binary-search (data-start actor-list) (data-end actor-list))))))

(defun file-size-string (bytes)
  "Returns a human-readable file size."
  (format nil "~3d MB" (floor bytes (* 1024 1024))))

(defun movie-line-p (current-entry movie-title)
  "Returns whether a given line matches the given movie."
  (when (>= (length current-entry) (length movie-title))
    (loop for movie-char across movie-title and
       entry-char across (string-left-trim "\"" current-entry)
       when (char/= movie-char entry-char) do (return-from movie-line-p)
       finally (return t))))

(defmethod inverse-search-partition
    ((actor-list actor-list) (movies cons) n i progress progress-changed)
  "Returns roles in a partition matching the specified movies."
  (when (< n 1) (error "at least one partition needed"))
  (when (or (< i 0) (>= i n)) (error "illegal partition index"))
  (let* ((results (make-hash-table :test 'equal))
	 (partition-step (floor (data-length actor-list) n))
	 (partition-start (+ (data-start actor-list) (* partition-step i)))
	 (partition-end (if (= i (- n 1))
			    (data-end actor-list)
			    (+ (data-start actor-list) (* partition-step (1+ i))))))
    (labels ((change-progress (new-progress)
	       (setf (elt progress i) new-progress)
	       (ccl:signal-semaphore progress-changed)))
      (with-open-actor-list actor-list
	(file-position stream partition-start)
	(read-until-record actor-list stream)
	(let ((current-actor "") (current-entry ""))
	  (do-lines stream ((lines 0 (1+ lines)))
	      (end-of-data-p actor-list (file-position stream))
	    (when (= (mod lines 500000) 0)
	      (change-progress (- (file-position stream) partition-start)))
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
		   (when (movie-line-p current-entry (title movie))
		     (handler-case
			 (let ((role (make-instance 'role
						    :actor (make-instance 'actor
									  :name current-actor)
						    :line current-entry)))
			   (when (movie= movie (movie role))
			     (push role (gethash movie results))))
		       (skip-role ()))))))))
      (change-progress -1))
    (loop for movie in movies do
	 (setf (gethash movie results)
	       (nreverse (delete-duplicates-in-sorted-list (gethash movie results)
							   :test #'role=)))
       finally (return results))))

(defmethod inverse-search ((actor-list actor-list) movie &optional (n 4))
  "Returns roles matching a specified movie."
  (gethash movie (inverse-search actor-list (list movie) n)))

(defmethod inverse-search ((actor-list actor-list) (movies cons) &optional (n 4))
  "Returns roles matching the specified movies."
  (assert movies)
  (show-notice actor-list)
  (format t "Inverse searching ~a for ~r movie~:*~p ...~%"
	  (file-name actor-list) (length movies))
  (labels ((fn (i progress progress-changed)
	     (inverse-search-partition actor-list movies n i progress progress-changed)))
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
				      (data-length actor-list))))
	   (ccl:wait-on-semaphore progress-changed))
      (format t "~&")
      (loop for process in processes do
	   (let ((process-results (ccl:join-process process)))
	     (loop for movie in movies do
		  (setf (gethash movie results)
			(append (gethash movie results) (gethash movie process-results))))))
      results)))