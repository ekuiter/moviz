(in-package :main)

(defclass movie-node (node movie) ())
(defclass actor-edge (edge actor) ())

(defmethod compare ((node-1 movie-node) (node-2 movie-node))
  (string<= (title node-1) (title node-2)))

(defmethod label ((node movie-node))
  (title node))

(defmethod label ((edge actor-edge))
  (name edge))

(defun intersect-fn (&key (test #'eql))
  "Returns a function that intersects two lists."
  (labels ((intersect (a b)
	     (cond ((null a) nil)
		   ((member (car a) b :test test) (cons (car a) (intersect (cdr a) b)))
		   (t (intersect (cdr a) b)))))
    #'intersect))

(defun intersect-many-fn (&key (test #'eql))
  "Returns a function that intersects any number of lists."
  (lambda (&rest lists)
    (reduce (lambda (a b) (funcall (intersect-fn :test test) a b)) lists)))

(defun intersect-movies (&rest movies)
  "Returns a list of actors who played in the specified movies."
  (let* ((actresses (make-instance 'actor-list :file-name "imdb/actresses.list"))
	 (results (inverse-search actresses movies)))
    (apply (intersect-many-fn :test #'actor=)
	   (mapcar (lambda (movie) (gethash movie results)) movies))))

(defun intersection-graph (intersection node-1 node-2)
  (let* ((graph (make-instance 'graph)))
    (add-node graph node-1)
    (add-node graph node-2)
    (loop for actor in intersection do
	 (add-edge graph (make-instance 'actor-edge :node-1 node-1
					:node-2 node-2
					:name (name actor))))
    graph))

(defun main ()
  (let ((hp2 (make-instance 'movie-node :title "Harry Potter and the Chamber of Secrets"))
	(hp3 (make-instance 'movie-node :title "Harry Potter and the Prisoner of Azkaban")))
    (show (intersection-graph (intersect-movies hp2 hp3) hp2 hp3))))

(defun example-graph ()
  (let* ((graph (make-instance 'graph))
	 (person-of-interest (make-instance 'movie-node :title "Person of Interest"))
	 (supernatural (make-instance 'movie-node :title "Supernatural"))
	 (house-of-cards (make-instance 'movie-node :title "House of Cards"))
	 (mr-robot (make-instance 'movie-node :title "Mr. Robot"))
	 (westworld (make-instance 'movie-node :title "Westworld")))
    (add-node graph (make-instance 'movie-node :title "Sherlock"))
    (add-edge graph (make-instance 'actor-edge :node-1 person-of-interest
				   :node-2 supernatural :name "Mark Pellegrino"))
    (add-edge graph (make-instance 'actor-edge :node-1 person-of-interest
				   :node-2 westworld :name "Jimmi Simpson"))
    (add-edge graph (make-instance 'actor-edge :node-1 person-of-interest
				   :node-2 house-of-cards :name "Jimmi Simpson"))
    (add-edge graph (make-instance 'actor-edge :node-1 westworld
				   :node-2 house-of-cards :name "Jimmi Simpson"))
    (add-edge graph (make-instance 'actor-edge :node-1 person-of-interest
				   :node-2 mr-robot :name "Michel Gill"))
    (add-edge graph (make-instance 'actor-edge :node-1 house-of-cards
				   :node-2 mr-robot :name "Michel Gill"))
    (add-edge graph (make-instance 'actor-edge :node-1 person-of-interest
				   :node-2 house-of-cards :name "Michel Gill"))
    graph))