(in-package :app)

(defclass movie-graph (graph) ())

(defclass movie-node (node movie)
  ((actors :initform :undefined)
   (actresses :initform :undefined)
   (actors-list :allocation :class
	       :initform (make-list-instance 'actors))
   (actresses-list :allocation :class
		 :initform (make-list-instance 'actresses))))

(defclass role-edge (edge)
  ((role-1 :initarg :role-1
	   :initform (error "must supply role 1")
	   :reader role-1)
   (role-2 :initarg :role-2
	   :initform (error "must supply role 2")
	   :reader role-2)
   (gender :initarg :gender
	   :initform nil
	   :reader gender)))

(defmacro defgraphclass (class superclasses)
  `(progn (defclass ,class ,superclasses ())
	  (defmethod initialize-instance :after ((graph ,class) &key parent-graph)
	    (unless parent-graph (error "must supply parent graph"))
	    (with-slots (vertices edges) graph
	      (setf vertices (slot-value parent-graph 'vertices)
		    edges (slot-value parent-graph 'edges))))
	  (defmethod ,(intern (concatenate 'string "MAKE-" (symbol-name class)))
	      ((parent-graph movie-graph))
	    (make-instance ',class :parent-graph parent-graph))))

(defmacro defgraph (class superclasses &body body)
  `(progn (defgraphclass ,class ,superclasses)
	  (defmethod make-edge ((graph ,class) node-1 node-2 edges)
	    (declare (ignorable node-1 node-2 edges))
	    (merge-plist (call-next-method)
			 (progn ,@body)))))

(defmacro make-graph (parent-graph &rest types)
  (unless types (return-from make-graph `(make-graph ,parent-graph unlabeled)))
  (let ((class (intern (format nil "~{~a-~}GRAPH" types)))
	(superclasses
	 (mapcar (lambda (type)
		   (intern (concatenate 'string (symbol-name type) "-GRAPH")))
		 types)))
    `(progn ,(unless (= (length types) 1) `(defgraphclass ,class ,superclasses))
	    (make-instance ',class :parent-graph ,parent-graph))))

(defmethod compare ((node-1 movie-node) (node-2 movie-node))
  (string<= (title node-1) (title node-2)))

(defmethod label ((node movie-node))
  (title node))

(defmethod label ((edge role-edge))
  (readable-name (actor (role-1 edge))))

(defmethod role-edge-score ((edge role-edge))
  (min (role-score (role-1 edge)) (role-score (role-2 edge))))

(defmethod role-edge< ((edge-1 role-edge) (edge-2 role-edge))
  (< (role-edge-score edge-1) (role-edge-score edge-2)))

(defmacro define-lazy-slot (slot class-slot)
  `(progn (defmethod ,slot ((node movie-node))
	    (with-slots (,slot ,class-slot) node
	      (if (eql ,slot :undefined)
		  (setf ,slot (inverse-search ,class-slot node))
		  ,slot)))
	  (defmethod ,slot ((nodes cons))
	    (assert nodes)
	    (let ((results (inverse-search (slot-value (first nodes) ',class-slot) nodes)))
	      (loop for node being the hash-keys in results using (hash-value node-results) do
		   (setf (slot-value node ',slot) node-results)))
	    nil)))

(define-lazy-slot actors actors-list)
(define-lazy-slot actresses actresses-list)

(defmethod total-actors ((node movie-node))
  (+ (length (actors node)) (length (actresses node))))

(defun intersect-sorted-fn (&key (test= #'eql) (test< #'<) (key #'identity))
  "Returns a function that intersects two sorted lists."
  (macrolet ((advance (x xs)
	       `(progn (setf ,x (car ,xs)) (setf ,xs (cdr ,xs)))))
    (lambda (list-a list-b)
      (if (and list-a list-b)
	  (loop with (a . as) = list-a and (b . bs) = list-b while (and a b)
	     if (funcall test= (funcall key a) (funcall key b))
	     collect (list a b) and do (advance a as) (advance b bs)
	     else if (funcall test< (funcall key a) (funcall key b)) do (advance a as)
	     else do (advance b bs))))))

(defun intersect-movie-nodes (accessor node-1 node-2)
  "Returns a list of actors or actresses who played in the specified movie nodes."
  (funcall (intersect-sorted-fn :test= #'actor= :test< #'actor< :key #'actor)
	   (funcall accessor node-1) (funcall accessor node-2)))

(defun load-nodes (&rest nodes)
  (assert nodes)
  (when (= (length nodes) 1)
    (actors (first nodes)) (actresses (first nodes)) (return-from load-nodes))
  (funcall #'actors nodes)
  (funcall #'actresses nodes)
  nil)

(defmethod add-node :around ((graph movie-graph) (node-1 movie-node))
  (load-nodes node-1)
  (call-next-method)
  (labels ((add-edges (node-1 node-2 accessor gender)
	     (loop for (role-1 role-2) in (intersect-movie-nodes accessor node-1 node-2) do
		  (add-edge graph
			    (make-instance 'role-edge :node-1 node-1 :node-2 node-2
					   :role-1 role-1 :role-2 role-2 :gender gender)))))
    (loop for node-2 in (vertices graph)
       unless (movie= node-1 node-2) do
	 (add-edges node-1 node-2 #'actors :male)
	 (add-edges node-1 node-2 #'actresses :female)))
  graph)

(defmethod add-nodes ((graph movie-graph) &rest nodes)
  (apply #'load-nodes nodes)
  (loop for node in nodes do
       (add-node graph node))
  graph)

(defun short-label (edges)
  (format nil "~a actors" (length edges)))

(defun map-number (x in-min in-max out-min out-max)
  (+ (float (/ (* (- x in-min) (- out-max out-min)) (- in-max in-min))) out-min))

(defun average (a b)
  (/ (+ a b) 2))

(defun merge-plist (p1 p2)
  (loop for (key value) on p1 by #'cddr
     unless (getf p2 key)
     do (push value p2) (push key p2))
  p2)

(defmethod make-edge ((graph movie-graph) node-1 node-2 edges)
  (declare (ignore node-1 node-2 edges))
  nil)

(defgraph unlabeled-graph (movie-graph)
  (list :label ""))

(defgraph detailed-graph (movie-graph)
  (list :label (format nil "~{~a~^~%~}" (mapcar #'label edges))))

(defgraph condensed-graph (detailed-graph)
   (when (> (length edges) 10) (list :label (short-label edges))))

(defgraph weighted-graph (movie-graph)
 (let ((weight (float (* (/ (length edges) ; rough percentage of common actors
			     (average (total-actors node-1) (total-actors node-2))) 100)))
	(min-weight 0.2) (max-weight 15))
    (setf weight (min max-weight (max min-weight weight)))
    (list :label (write-to-string (length edges)) :color "\"#555555\""
	  :weight weight :penwidth weight)))

(defgraph top-actors-graph (detailed-graph)
  (list :label (format nil "~{~a~^~%~}"
		       (loop for edge in edges repeat 5	while (<= (role-edge-score edge) 15)
			  collect (label edge)))))

(defmethod to-dot ((graph movie-graph) &key (stream t))
  (labels ((init-fn ()
	     (let ((font-name "Helvetica"))
	       (format stream "node [fontname=\"~a\"];~
                               edge [fontname=\"~:*~a\"][fontsize=12];" font-name)))
	   (edge-fn (make-edge-fn)
	     (loop for (node-1 node-2) being the hash-keys in (slot-value graph 'edges)
		using (hash-value edges) do
		  (setf edges (sort (copy-list edges) #'role-edge<))
		  (handler-case
		      (apply make-edge-fn node-1 node-2 (make-edge graph node-1 node-2 edges))
		    (label-too-long-error ()
		      (funcall make-edge-fn node-1 node-2 :label (short-label edges)))))))
    (call-next-method graph :stream stream :init-fn #'init-fn :edge-fn #'edge-fn)))

(defmethod make-image ((graph movie-graph) file-name &key (format "png"))
  (call-next-method graph file-name :format format :charset "latin1"))

(defmethod show ((graph movie-graph) &key (open t) (format "png"))
  (call-next-method graph :open open :format format :charset "latin1"))

(defvar *graph* (make-instance 'movie-graph))

(defun current-graph ()
  *graph*)

(defun clear-graph ()
  (setf *graph* (make-instance 'movie-graph)))

(defun add-movies (&rest movies)
  (apply #'add-nodes
	 (append (list *graph*)
		 (mapcar (lambda (movie) (make-instance 'movie-node :title movie)) movies))))

(defun save-and-quit (file-name)
  (ccl:save-application file-name :prepend-kernel t))
