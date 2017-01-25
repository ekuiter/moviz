(defparameter *directory* "~/graph")
(defparameter *dot-command* "/usr/local/bin/dot")

;;; Helpers

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun run (command &key (wait t) (input nil))
  (let ((shell-command (concatenate 'string "cd " *directory* ";" command)))
    (ccl:run-program "sh" (list "-c" shell-command) :wait wait :input input)))

;;; Classes

(defclass graph ()
  ((vertices :initform nil
	     :reader vertices)
   (edges :initform (make-hash-table :test 'equal))))

(defclass node () ())

(defclass edge ()
  ((node-1 :initarg :node-1
	   :initform (error "must supply node 1")
	   :reader node-1)
   (node-2 :initarg :node-2
	   :initform (error "must supply node 2")
	   :reader node-2)))

(defclass series-node (node)
  ((title :initarg :title
	  :initform (error "must supply title")
	  :reader title)))

(defclass actor-edge (edge)
  ((actor :initarg :actor
	  :initform (error "must supply actor")
	  :reader actor)))

(defgeneric compare (node-1 node-2))
(defgeneric label (object))

;;; Graph methods

(defmethod print-object ((graph graph) stream)
  (print-unreadable-object (graph stream :type t)
    (format stream "{~{~a~^, ~}}, {~{~a~^, ~}}"
	    (slot-value graph 'vertices) (edges graph))))

(defmethod edges ((graph graph))
  (flatten (loop for edges being the hash-values in (slot-value graph 'edges)
	      collecting edges)))

(defmethod add-node ((graph graph) (node node))
  (with-slots (vertices) graph
    (unless (find node vertices)
      (push node vertices))))

(defmethod add-edge ((graph graph) (edge edge))
  (with-slots (vertices edges) graph
    (with-slots (node-1 node-2) edge
      (add-node graph node-1)
      (add-node graph node-2)
      (push edge (gethash (to-key edge) edges)))))

(defmethod to-dot ((graph graph) &key (stream t))
  (format stream "graph {~%")
  (loop for node in (vertices graph) do
       (format stream "\"~a\";~%" (label node)))
  (loop for edge in (edges graph) do
       (format stream "\"~a\" -- \"~a\" [label=\"~a\"];~%"
	       (label (node-1 edge)) (label (node-2 edge)) (label edge)))
  (format stream "}"))

(defmethod make-png ((graph graph) filename)
  (let* ((process (run (concatenate 'string *dot-command* " -Tpng -o " filename)
		       :wait nil :input :stream))
	 (stream (ccl:external-process-input-stream process)))
    (to-dot graph :stream stream)
    (close stream)
    (loop while (equal (ccl:external-process-status process) :running))))

(defmethod open-png ((graph graph))
  (make-png graph "graph.png")
  (run "open graph.png"))

;;; Node methods

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~a" (label node))))

(defmethod compare ((node-1 node) (node-2 node))
  t)

(defmethod label ((node node))
  nil)

;;; Edge methods

(defmethod initialize-instance :after ((edge edge) &key)
  (with-slots (node-1 node-2) edge
    (unless (typep node-1 'node) (error "node-1 is not a node"))
    (unless (typep node-2 'node) (error "node-2 is not a node"))
    (let* ((node-1-before-2 (compare node-1 node-2))
	   (tmp (if node-1-before-2 node-2 node-1)))
      (setf node-1 (if node-1-before-2 node-1 node-2))
      (setf node-2 tmp))))

(defmethod print-object ((edge edge) stream)
  (with-slots (node-1 node-2) edge
    (print-unreadable-object (edge stream :type t)
      (format stream "~a -- ~a, ~a" node-1 node-2 (label edge)))))

(defmethod to-key ((edge edge))
  (with-slots (node-1 node-2) edge
    (list node-1 node-2)))

(defmethod label ((edge edge))
  nil)

;;; Series node methods

(defmethod compare ((node-1 series-node) (node-2 series-node))
  (string<= (title node-1) (title node-2)))

(defmethod label ((node series-node))
  (title node))

;;; Actor edge methods

(defmethod label ((edge actor-edge))
  (actor edge))

;;; Main example

(defun example-graph ()
  (let* ((graph (make-instance 'graph))
	 (person-of-interest (make-instance 'series-node :title "Person of Interest"))
	 (supernatural (make-instance 'series-node :title "Supernatural"))
	 (house-of-cards (make-instance 'series-node :title "House of Cards"))
	 (mr-robot (make-instance 'series-node :title "Mr. Robot"))
	 (westworld (make-instance 'series-node :title "Westworld")))
    (add-node graph (make-instance 'series-node :title "Sherlock"))
    (add-edge graph (make-instance 'actor-edge :node-1 person-of-interest
				   :node-2 supernatural :actor "Mark Pellegrino"))
    (add-edge graph (make-instance 'actor-edge :node-1 person-of-interest
				   :node-2 westworld :actor "Jimmi Simpson"))
    (add-edge graph (make-instance 'actor-edge :node-1 person-of-interest
				   :node-2 house-of-cards :actor "Jimmi Simpson"))
    (add-edge graph (make-instance 'actor-edge :node-1 westworld
				   :node-2 house-of-cards :actor "Jimmi Simpson"))
    (add-edge graph (make-instance 'actor-edge :node-1 person-of-interest
				   :node-2 mr-robot :actor "Michel Gill"))
    (add-edge graph (make-instance 'actor-edge :node-1 house-of-cards
				   :node-2 mr-robot :actor "Michel Gill"))
    (add-edge graph (make-instance 'actor-edge :node-1 person-of-interest
				   :node-2 house-of-cards :actor "Michel Gill"))
    graph))

(defun main ()
  (open-png (example-graph)))