(defparameter *directory* "~/graph")
(defparameter *dot-command* "/usr/local/bin/dot")

(defun make-graph ()
  (make-hash-table :test 'equal))

(defun node-pair (node1 node2 &key (test 'string<=))
  (let ((test-result (funcall test node1 node2)))
    (list (if test-result node1 node2)
	  (if test-result node2 node1))))

(defun flatten (l &optional (depth 1))
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l
	      appending (if (> depth 0)
			    (flatten a (- depth 1))
			    (list a))))))

(defun all-edges (graph)
  (flatten (loop for node-pair being the hash-keys in graph using (hash-value edges)
	      collecting (mapcar (lambda (edge) (append node-pair (list edge))) edges))))

(defun edges (graph node1 node2)
  (gethash (node-pair node1 node2) graph))

(defun insert-edge (graph node1 node2 edge)
  (push edge (gethash (node-pair node1 node2) graph)))

(defun example-graph ()
  (let ((graph (make-graph)))
    (insert-edge graph "Person of Interest" "Supernatural" "Mark Pellegrino")
    (insert-edge graph "Person of Interest" "Westworld" "Jimmi Simpson")
    (insert-edge graph "Person of Interest" "House of Cards" "Jimmi Simpson")
    (insert-edge graph "Westworld" "House of Cards" "Jimmi Simpson")
    (insert-edge graph "Person of Interest" "Mr. Robot" "Michel Gill")
    (insert-edge graph "House of Cards" "Mr. Robot" "Michel Gill")
    (insert-edge graph "Person of Interest" "House of Cards" "Michel Gill")
    graph))

(defun run (command &key (wait t) (input nil))
  (let ((shell-command (concatenate 'string "cd " *directory* ";" command)))
    (ccl:run-program "sh" (list "-c" shell-command) :wait wait :input input)))

(defun to-dot (graph &key (stream nil))
  (format stream "graph {~%~{~{\"~a\" -- \"~a\" [label=\"~a\"];~%~}~}}" (all-edges graph)))

(defun make-png (graph filename)
  (let* ((process (run (concatenate 'string *dot-command* " -Tpng -o " filename)
		       :wait nil :input :stream))
	 (stream (ccl:external-process-input-stream process)))
    (to-dot graph :stream stream)
    (close stream)
    (loop while (equal (ccl:external-process-status process) :running))))

(defun open-png (graph)
  (make-png graph "graph.png")
  (run "open graph.png"))

(defun main ()
  (open-png (example-graph)))