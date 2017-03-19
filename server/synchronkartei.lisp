(in-package :synchronkartei)

(defparameter +root-url+ "https://www.synchronkartei.de")

(defclass dubbed-movie (imdb:movie)
  ((path :initarg :path
	 :initform (error "Must supply path")
	 :reader path)))

(defclass voice-actor (imdb:actor) ())

(defclass dubbed-role ()
  ((role :initarg :role
	 :initform (error "Must supply role")
	 :reader role)
   (voice-actor :initarg :voice-actor
	  :initform (error "Must supply voice actor")
	  :reader voice-actor)))

(defmethod print-object ((dubbed-role dubbed-role) stream)
  "Prints a dubbed role."
  (print-unreadable-object (dubbed-role stream :type t)
    (format stream "~a dubs ~a" (imdb:name (voice-actor dubbed-role)) (role dubbed-role))))

(defun to-list (vector)
  (map 'list #'identity vector))

(defmacro $ (doc &rest args)
  (let ((sym (gensym)))
    `(let ((,sym ,doc))
       (to-list (lquery:$ ,sym ,@args)))))

(defun fetch (url &optional params)
  (lquery:$
    (lquery:initialize
     (handler-bind ((flexi-streams:external-format-encoding-error
		     (lambda (c) (declare (ignore c)) (use-value #\?))))
       (drakma:http-request (concatenate 'string +root-url+ url) :parameters params)))))

(defun do-search (movie-title)
  (let* ((headings ($ (fetch "/suche" (acons "q" movie-title nil)) ".synchro-main h2"))
	 (headings (remove-if-not (lambda (heading-text) (or (search "Filme " heading-text)
							     (search "Serien " heading-text)))
				  headings :key #'plump:text))
	 (links ($ (loop for heading in headings append
			($ (plump:next-element heading) "li")) "a")))
    (mapcar (lambda (link) (make-instance 'dubbed-movie :title (plump:text link)
					  :path (plump:attribute link "href")))
	    links)))

(defmethod find-role-for-actor ((node app:movie-node) actor accessor)
  (find actor (funcall accessor node) :test #'imdb:readable-actor= :key #'imdb:actor))

(defmethod voice-actors ((movie dubbed-movie) (node app:movie-node))
  (let ((response ($ (fetch (path movie)) ".synchro-main table tbody tr")))
    (time
     (loop for table-row in response
	for table-cells = ($ table-row "td")
	for actor-link = (first ($ (first table-cells) "a"))
	for voice-actor-link = (first ($ (second table-cells) "a"))
	when (and actor-link voice-actor-link) append
	  (let* ((actor (make-instance 'imdb:actor :name (plump:text (plump:trim actor-link))))
		 (voice-actor (make-instance 'voice-actor
					     :name (plump:text (plump:trim voice-actor-link))))
		 (role (or (find-role-for-actor node actor #'app:actors)
			   (find-role-for-actor node actor #'app:actresses))))
	    (when role
	      (list (make-instance 'dubbed-role :role role :voice-actor voice-actor))))))))
