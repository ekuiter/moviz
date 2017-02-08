(in-package :main)

(defclass movie-graph (graph) ())

(defclass movie-node (node movie)
  ((actors :initform nil)
   (actresses :initform nil)
   (actor-list :allocation :class
	       :initform (make-instance 'actor-list :file-name "imdb/actors.list"))
   (actress-list :allocation :class
		 :initform (make-instance 'actor-list :file-name "imdb/actresses.list"))))

(defclass actor-edge (edge actor)
  ((gender :initarg :gender
	   :initform nil
	   :reader gender)))

(defmethod compare ((node-1 movie-node) (node-2 movie-node))
  (string<= (title node-1) (title node-2)))

(defmethod label ((node movie-node))
  (title node))

(defmethod gender-string ((edge actor-edge))
  (when (gender edge)
    (if (eql (gender edge) :male) "♂" "♀")))

(defmethod label ((edge actor-edge))
  (format nil "~@[~a~] ~a" (gender-string edge) (name edge)))

(defmacro define-lazy-slot (slot class-slot)
  `(progn (defmethod ,slot ((node movie-node))
	    (with-slots (,slot ,class-slot) node
	      (or ,slot
		  (setf ,slot (inverse-search ,class-slot node)))))
	  (defmethod ,slot ((nodes cons))
	    (assert nodes)
	    (let ((results (inverse-search (slot-value (first nodes) ',class-slot) nodes)))
	      (loop for node being the hash-keys in results using (hash-value node-results) do
		   (setf (slot-value node ',slot) node-results)))
	    nil)))

(define-lazy-slot actors actor-list)
(define-lazy-slot actresses actress-list)

(defun intersect-sorted-fn (&key (test= #'eql) (test< #'<))
  "Returns a function that intersects two sorted lists."
  (macrolet ((advance (x xs)
	       `(progn (setf ,x (car ,xs)) (setf ,xs (cdr ,xs)))))
    (lambda (list-a list-b)
      (loop with (a . as) = list-a and (b . bs) = list-b while (and a b)
	 if (funcall test= a b) collect a and do (advance a as) (advance b bs)
	 else if (funcall test< a b) do (advance a as)
	 else do (advance b bs)))))

(defun intersect-movie-nodes (accessor node-1 node-2)
  "Returns a list of actors or actresses who played in the specified movie nodes."
  (funcall (intersect-sorted-fn :test= #'actor= :test< #'actor<)
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
	     (loop for actor in (intersect-movie-nodes accessor node-1 node-2) do
		  (add-edge graph (make-instance 'actor-edge :node-1 node-1 :node-2 node-2
						 :name (name actor) :gender gender)))))
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

(defmethod show ((graph movie-graph) &key (open t) (condensed nil))
  (call-next-method graph :open open :mode :merge :condensed condensed))

(defvar *graph* (make-instance 'movie-graph))

(defun clear-graph ()
  (setf *graph* (make-instance 'movie-graph)))

(defun add-movies (&rest movies)
  (apply #'add-nodes
	 (append (list *graph*)
		 (mapcar (lambda (movie) (make-instance 'movie-node :title movie)) movies))))

(defun save-and-quit (file-name)
  (ccl:save-application file-name :prepend-kernel t))