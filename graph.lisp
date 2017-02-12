(in-package :graph)

(defparameter +dot-command+ "/usr/local/bin/dot")

;;; Helpers

(defun run (command &key (wait t) (input nil) (output nil))
  (let ((shell-command (concatenate 'string "cd "
				    (namestring (ccl:current-directory)) ";" command)))
    (ccl:run-program "sh" (list "-c" shell-command) :wait wait :input input :output output)))

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

(defgeneric compare (node-1 node-2))
(defgeneric label (object))

;;; Graph methods

(defmethod print-object ((graph graph) stream)
  (print-unreadable-object (graph stream :type t)
    (format stream "{~{~a~^, ~}}, "
		  (slot-value graph 'vertices))
    (let ((edges (edges graph)))
      (if (> (length edges) 30)
	  (format stream "~a edge~:*~p" (length edges))
	  (format stream "{~{~a~^, ~}}" (edges graph))))))

(defmethod edges ((graph graph))
  (loop for edges being the hash-values in (slot-value graph 'edges) appending edges))

(defmethod add-node ((graph graph) (node node))
  (with-slots (vertices) graph
    (unless (find node vertices)
      (push node vertices))))

(defmethod add-edge ((graph graph) (edge edge))
  (with-slots (vertices edges) graph
    (with-slots (node-1 node-2) edge
      (unless (find node-1 vertices) (add-node graph node-1))
      (unless (find node-2 vertices) (add-node graph node-2))
      (push edge (gethash (to-key edge) edges)))))

(define-condition label-too-long-error (error) ())

(defmethod to-dot ((graph graph) &key (stream t) init-fn edge-fn)
  (format stream "graph {~%")
  (when init-fn (funcall init-fn))
  (loop for node in (vertices graph) do
       (format stream "\"~a\";~%" (label node)))
  (labels ((make-edge (node-1 node-2 &rest options)
	     (when (getf options :label)
	       (when (> (length (getf options :label)) 16384)
		 (error 'label-too-long-error)) ; GraphViz weirdness
	       (setf (getf options :label) (format nil "\"~a\"" (getf options :label))))
	     (format stream "\"~a\" -- \"~a\" ~{[~(~a~)=~a]~};~%"
		     (label node-1) (label node-2) options)))
    (if edge-fn (funcall edge-fn #'make-edge)
	(loop for edge in (edges graph) do
	     (make-edge (node-1 edge) (node-2 edge) :label (label edge))))
    (format stream "}")))

(defmethod make-image ((graph graph) file-name &key (format "png") charset)
  (let* ((command (concatenate 'string +dot-command+ " -T" format " -o" file-name))
	 (process (run (if charset (concatenate 'string command " -Gcharset=" charset) command)
		       :wait nil :input :stream :output *terminal-io*))
	 (stream (ccl:external-process-input-stream process)))
    (to-dot graph :stream stream)
    (close stream)
    (loop while (equal (ccl:external-process-status process) :running))))

;;; SLIME: insert the following to ~/.emacs
;;; (setq slime-enable-evaluate-in-emacs t)
;;; (eval-after-load "slime"
;;;   '(progn (slime-setup '(slime-media))))
;;; iTerm2: https://raw.githubusercontent.com/gnachman/iTerm2/master/tests/imgcat

(defun temporary-file-name (prefix format &key open)
  (concatenate 'string (if open prefix
			   #+swank (swank:eval-in-emacs
				    `(make-temp-name (concat temporary-file-directory ,prefix)))
			   #-swank prefix) "." format))

(defmethod show ((graph graph) &key (format "png") charset open)
  (let ((file-name (temporary-file-name "graph" format :open open)))
    (make-image graph file-name :format format :charset charset)
    (when open
      (run (concatenate 'string "open " file-name)))
    (unless open
      #+:swank (swank:eval-in-emacs
		`(slime-media-insert-image (create-image ,file-name) ,file-name))
      #-:swank (run (concatenate 'string "imgcat " file-name) :output t)
      #+:swank (swank:eval-in-emacs `(delete-file ,file-name))
      #-:swank (delete-file file-name)))
  nil)

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
      (format stream "~a -- ~a (~a)" node-1 node-2 (label edge)))))

(defmethod to-key ((edge edge))
  (with-slots (node-1 node-2) edge
    (list node-1 node-2)))

(defmethod label ((edge edge))
  nil)
