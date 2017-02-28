(in-package :server)

(defparameter +graph-path+ "assets/graph.svg")
(defvar *classes* nil)

(defmacro defroute (options (bind-request bind-response &optional bind-args) &body body)
  `(wookie:defroute ,options (,bind-request ,bind-response args)
     (destructuring-bind ,bind-args args ,@body)))

(defmacro defsearchroute (path function id-class-function)
  `(defroute (:get ,(format nil "/~a/(.+)/(.+)" path)) (req res (list-string id-string))
     (let* ((list (imdb:make-list-instance list-string))
	    (id-object (make-instance (,id-class-function list) :string id-string))
	    (results (,function list id-object)))
       (send-response res :headers '(:content-type "application/json")
		      :body (json:encode-json-to-string results)))))

(defun redirect (res path)
  (send-response res :status 302 :headers (list :location path)))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (s nil :prologue t) ,@body))

(defmacro make-html (&body body)
  `(with-html-string (:html (:head (:title "movie-graph")
				   (:link :rel :stylesheet :href "assets/style.css"))
			    (:body ,@body))))

(defun update-graph ()
  (let ((graph (eval `(app:make-graph (app:current-graph) ,@*classes*))))
    (app:make-image graph +graph-path+ :format "svg")))

(defmacro def-graph-route (options (bind-request bind-response &optional bind-args) &body body)
  `(defroute ,options (,bind-request ,bind-response ,bind-args)
     ,@body
     (update-graph)
     (redirect ,bind-response "/")))

(load-plugins)
(setf (cl-who:html-mode) :html5)

(defroute (:get "/") (req res)
  (send-response res :headers '(:content-type "text/html; charset=utf-8")
		 :body (make-html
			 (:h1 "movie-graph")
			 (:p (:a :href "/clear/" "Clear")
			     (:form :method :get :action "/add/"
				    (:input :name "movies")
				    (:input :type :submit :value "Add"))
			     (:form :method :get :action "/update/"
				    (:input :name "classes")
				    (:input :type :submit :value "Update")))
			 (:object :data +graph-path+ :type "image/svg+xml"))))

(def-graph-route (:get "/update/(.*)") (req res (classes))
  (setf classes (or (get-var req "classes") classes))
  (setf *classes* (mapcar (lambda (class) (intern (string-upcase class) :app))
			  (split-sequence #\/ classes :remove-empty-subseqs t))))

(def-graph-route (:get "/clear/") (req res)
  (app:clear-graph))

(def-graph-route (:get "/add/(.*)") (req res (movies))
  (setf movies (or (get-var req "movies") movies))
  (apply #'app:add-movies (split-sequence #\/ movies :remove-empty-subseqs t)))

(defsearchroute "search" imdb:do-search imdb:id-class)
(defsearchroute "inverse-search" imdb:inverse-search imdb:inverse-id-class)

(def-directory-route "/assets" "./assets")

(defroute (:* ".+" :priority -1) (req res)
  (send-response res :body "Page not found." :status 404))

(defun serve (&optional (port 3000))
  (as:with-event-loop ()
    (let* ((listener (make-instance 'listener :bind "127.0.0.1" :port port))
	   (server (start-server listener)))
      (as:signal-handler 2 (lambda (sig)
			     (declare (ignore sig))
			     (as:free-signal-handler 2)
			     (as:close-tcp-server server))))))
