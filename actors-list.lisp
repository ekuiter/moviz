(in-package :imdb)

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

(defclass actors-list (imdb-list) ())

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

(defmethod readable-name ((actor actor))
  "Returns an actor's name in a readable form."
  (let ((last-name (unless (equal (last-name actor) "") (last-name actor))))
    (format nil "~a~@[ ~a~]" (first-name actor) last-name)))

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

(define-condition bad-line-error (error) ())

(defun line-to-parts (line)
  "Extracts a role's parts from an actor file line."
  (cl-ppcre:register-groups-bind (title voice name billing)
      ("\"?(.*?)\"? \\([\\d?]{4}(?:.*(\\(voice\\))|(?:.*\\[(.*?)\\])?(?:.*<(.*?)>)?)" line)
    (when voice
      (error 'bad-line-error))
    (values title name billing)))

(defmethod initialize-instance :after ((role role) &key line)
  "Initializes a role."
  (when line
    (multiple-value-bind (title name billing) (line-to-parts line)
      (setf (slot-value role 'movie) (make-instance 'movie :title title)
	    (slot-value role 'name) name
	    (slot-value role 'billing) (when billing (parse-integer billing)))))
  (unless (movie role)
    (error "Must supply movie")))

(defmethod print-object ((role role) stream)
  "Prints a role."
  (print-unreadable-object (role stream :type t)
    (with-slots (actor movie name) role
      (format stream "~a in ~a~@[ as ~a~]" (name actor) (title movie) name))))

(defmethod role-score ((role role))
  "Scores a role."
  (if (billing role) (billing role) 9999))

(defmethod role= ((role-1 role) (role-2 role))
  "Tests whether two roles are equal."
  (and (actor= (actor role-1) (actor role-2))
       (movie= (movie role-1) (movie role-2))))

(defmethod role< ((role-1 role) (role-2 role))
  "Tests whether a role is less than another."
  (< (role-score role-1) (role-score role-2)))

(define-lazy-slot actors-list data-start
    "Returns the offset of the first record."
  (do-lines stream () (null line)
    (when (or (equal line "THE ACTRESSES LIST")
	      (equal line "THE ACTORS LIST"))
      (dotimes (i 4)
	(read-line stream nil))
      (return (file-position stream)))))

(define-lazy-slot actors-list data-end
    "Returns the offset after the last record."
  (file-position stream (- (imdb::file-length instance) 100000))
  (do-lines stream () (null line)
    (when (equal line "SUBMITTING UPDATES")
      (dotimes (i 3)
	(back-line stream))
      (return (file-position stream)))))

(defun actor-line-p (line)
  "Returns whether a given line is the start of a record."
  (and (> (length line) 0) (char/= (aref line 0) #\Tab)))

(defmethod read-until-record ((actors-list actors-list) stream)
  "Advances the stream to the next record and returns the next record's actor."
  (back-char stream)
  (unless (char= (read-char stream) #\Newline)
    (read-line stream nil))
  (let ((begin-of-line (file-position stream)))
    (do-lines stream () (null line)
      (when (actor-line-p line)
	(file-position stream begin-of-line)
	(return-from read-until-record
	  (if (end-of-data-p actors-list begin-of-line)
	      nil
	      (subseq line 0 (position #\Tab line)))))
      (setf begin-of-line (file-position stream)))))

(defun delete-duplicates-in-sorted-list (list &key (test #'eql))
  "Deletes all duplicates in a sorted list (more efficient than delete-duplicates)."
  (mapcon (lambda (cell)
	    (when (or (null (cdr cell)) (not (funcall test (car cell) (cadr cell))))
	      (list (car cell)))) list))

(defmethod read-record ((actors-list actors-list) actor stream)
  "Advances the stream to the next record and returns the current record."
  (when (end-of-data-p actors-list (file-position stream))
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
					  (bad-line-error ())))
			 (split-sequence:split-sequence #\Tab record :remove-empty-subseqs t)))
    (delete-duplicates-in-sorted-list record :test #'role=)))

(defmethod do-search ((actors-list actors-list) (actor actor))
  "Returns the record for a specified actor."
  (show-notice actors-list)
  (with-open-list actors-list
    (let ((actor-name (name actor)))
      (labels ((binary-search (min max)
		 (when (> min max)
		   (return-from binary-search nil))
		 (let ((mid (floor (+ min max) 2)))
		   (file-position stream mid)
		   (let* ((current-actor (read-until-record actors-list stream))
			  (less (string<= actor-name current-actor))
			  (new-min (if less min mid))
			  (new-max (if less mid max)))
		     (when (and (= min new-min) (= max new-max))
		       (return-from binary-search nil))
		     (when (equal actor-name current-actor)
		       (return-from binary-search (read-record actors-list actor stream)))
		     (file-position stream new-min)
		     (binary-search new-min new-max)))))
	(file-position stream (data-start actors-list))
	(binary-search (data-start actors-list) (data-end actors-list))))))

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
    ((actors-list actors-list) (movies cons) n i progress progress-changed)
  "Returns roles in a partition matching the specified movies."
  (when (< n 1) (error "at least one partition needed"))
  (when (or (< i 0) (>= i n)) (error "illegal partition index"))
  (let* ((results (make-hash-table :test 'equal))
	 (partition-step (floor (data-length actors-list) n))
	 (partition-start (+ (data-start actors-list) (* partition-step i)))
	 (partition-end (if (= i (- n 1))
			    (data-end actors-list)
			    (+ (data-start actors-list) (* partition-step (1+ i))))))
    (labels ((change-progress (new-progress)
	       (setf (elt progress i) new-progress)
	       (ccl:signal-semaphore progress-changed)))
      (with-open-list actors-list
	(file-position stream partition-start)
	(read-until-record actors-list stream)
	(let ((current-actor "") (current-entry ""))
	  (do-lines stream ((lines 0 (1+ lines)))
	      (end-of-data-p actors-list (file-position stream))
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
		       (bad-line-error ()))))))))
      (change-progress -1))
    (loop for movie in movies do
	 (setf (gethash movie results)
	       (nreverse (delete-duplicates-in-sorted-list (gethash movie results)
							   :test #'role=)))
       finally (return results))))

(defmethod inverse-search ((actors-list actors-list) movie &optional (n 4))
  "Returns roles matching a specified movie."
  (gethash movie (inverse-search actors-list (list movie) n)))

(defmethod inverse-search ((actors-list actors-list) (movies cons) &optional (n 4))
  "Returns roles matching the specified movies."
  (assert movies)
  (show-notice actors-list)
  (format t "Inverse searching ~a for ~r movie~:*~p ...~%"
	  (file-name actors-list) (length movies))
  (labels ((fn (i progress progress-changed)
	     (inverse-search-partition actors-list movies n i progress progress-changed)))
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
				      (data-length actors-list))))
	   (ccl:wait-on-semaphore progress-changed))
      (format t "~&")
      (loop for process in processes do
	   (let ((process-results (ccl:join-process process)))
	     (loop for movie in movies do
		  (setf (gethash movie results)
			(append (gethash movie results) (gethash movie process-results))))))
      results)))