(in-package :json-helpers)

(defun allocate-and-populate-instance (class bindings)
  "See make-and-populate-instance in cl-json/src/objects.lisp."
  (let ((object (allocate-instance class)))
    (if (typep class 'fluid-class)
        (loop for slot in (json::class-direct-slots class)
	   for slot-name = (json::slot-definition-name slot)
	   if (and (slot-boundp object slot-name)
		   (null (slot-value object slot-name)))
	   do (slot-makunbound object slot-name)))
    (loop for (slot . value) in bindings
       if (slot-exists-p object slot)
       do (setf (slot-value object slot) value))
    object))

(defmacro bypass-initialization (class &optional (var 'instance) &body body)
  `(defmethod make-object (bindings (class (eql ',class)) &optional superclasses)
     (declare (ignore superclasses))
     (let ((,var (allocate-and-populate-instance (find-class class) bindings)))
       ,@body
       ,var)))

(defmacro encode-with-prototype (class &optional prototype (var 'instance) &body body)
  `(defmethod encode-json ((,var ,class) &optional stream)
     (with-object (stream)
       (encode-object-member :prototype ,(if prototype
					     (apply #'make-instance 'prototype prototype)
					     `(make-object-prototype ,var))
			     stream)
       ,@(or body
	     (list `(json::map-slots (stream-object-member-encoder stream) instance))))))

(defmacro make-decodable (&rest clauses)
  `(progn
     ,@(loop for clause in clauses
	  do (unless (listp clause) (setf clause (list clause))) collect 
	    `(progn (bypass-initialization ,(first clause) ,@(getf (rest clause) :decode))
		    (encode-with-prototype ,(first clause) ,@(getf (rest clause) :encode))))))

(defclass json-hash-table ()
  ((hash-table :initarg :hash-table)))

(defmethod make-object (bindings (class (eql 'json-hash-table)) &optional superclasses)
  (declare (ignore superclasses))
  (let ((hash-table (make-hash-table :test #'equal)))
    (loop for (nil . value) in bindings do
	 (setf (gethash (first value) hash-table) (rest value)))
    hash-table))

(encode-with-prototype json-hash-table nil instance
  (with-slots (hash-table) instance
    (maphash (lambda (key value)
	       (funcall (stream-object-member-encoder stream)
			key (append (list key) value)))
	     hash-table)))

(defmacro with-decoder-custom-semantics (&body body)
  `(with-shadowed-custom-vars
     (set-decoder-simple-clos-semantics)
     (set-custom-vars :array-type 'list)
     ,@body))

(defun decode-object-from-string (str)
  (with-decoder-custom-semantics (json:decode-json-from-string str)))

(defun test-decodable (obj)
  (decode-object-from-string
   (let ((json (json:encode-json-to-string obj)))
     (format t "~a~%" (subseq json 0 (min (length json) 1000)))
     json)))

(defun to-form (json &key to-symbol packages)
  (let ((*string* nil))
    (declare (special *string*))
    (flet ((beginning () (setf *string* (make-array 0 :fill-pointer 0
						    :adjustable t :element-type 'character)))
	   (parse-token (token) (vector-push-extend token *string*))
	   (end ()
	     (let ((string-upcase (string-upcase *string*)))
	       (if (and to-symbol (search to-symbol string-upcase))
		   (or (loop for package in packages
			  for symbol = (find-symbol string-upcase package)
			  if symbol return symbol finally (return nil))
		       (error "the symbol ~a does not exist" *string*))
		   *string*))))
      (bind-custom-vars
	  (:beginning-of-string #'beginning :string-char #'parse-token
				:end-of-string #'end :string-scope '(*string*))
	(decode-json-from-string json)))))

(defun build-object (key value object)
  (if value
      (acons key value object)
      object))
