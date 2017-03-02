(in-package :server)

(defparameter +graph-path+ "assets/graph.svg")
(defvar *graph-classes* nil)
(defvar *node-filter* (graph:all-filter))
(defvar *edge-filter* (graph:all-filter))

(load-plugins)
(setf (cl-who:html-mode) :html5)

(defmacro defroute (options (bind-request bind-response &optional bind-args) &body body)
  `(wookie:defroute ,options (,bind-request ,bind-response args)
     (destructuring-bind ,bind-args args ,@body)))

(defun send-json-response (res json)
  (send-response res :headers '(:content-type "application/json")
		 :body (json:encode-json-to-string json)))

(defmacro defsearchroute (path function id-class-function)
  `(defroute (:get ,(format nil "/~a/(.+)/(.+)" path)) (req res (list-string id-string))
     (let* ((list (imdb:make-list-instance list-string))
	    (id-object (make-instance (,id-class-function list) :string id-string))
	    (results (,function list id-object)))
       (send-json-response res results))))

(defun redirect (res path)
  (send-response res :status 302 :headers (list :location path)))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (s nil :prologue t) ,@body))

(defmacro make-html (&body body)
  `(with-html-string
     (:html (:head (:title "movie-graph")
		   (:link :rel :stylesheet :href "assets/app.css")
		   (:link :rel :stylesheet :href "assets/jquery-ui.min.css")
		   (:script :src "assets/jquery.min.js")
		   (:script :src "assets/jquery-ui.min.js")
		   (:script :src "assets/helpers.js")
		   (:script :src "assets/server.js")
		   (:script :src "assets/node-filter.js")
		   (:script :src "assets/app.js"))
	    (:body ,@body))))

(defun update-graph (&key (graph-classes *graph-classes*) (node-filter *node-filter*)
		       (edge-filter *edge-filter*))
  (let ((graph (eval `(app:make-graph (app:current-graph) ,@graph-classes))))
    (setf graph (graph:filter-nodes graph node-filter)
	  graph (graph:filter-edges graph edge-filter)
	  *graph-classes* graph-classes *node-filter* node-filter *edge-filter* edge-filter)
    (app:make-image graph +graph-path+ :format "svg")))

(defmacro def-graph-route (options (bind-request bind-response &optional bind-args) &body body)
  `(defroute ,options (,bind-request ,bind-response ,bind-args)
     (apply #'update-graph (progn ,@body))
     (send-response res :body "")))

(defun json-to-filter (json)
  (let ((*string* nil))
    (declare (special *string*))
    (flet ((beginning () (setf *string* (make-array 0 :fill-pointer 0
						    :adjustable t :element-type 'character)))
	   (parse-token (token) (vector-push-extend token *string*))
	   (end ()
	     (let ((string-upcase (string-upcase *string*)))
	       (if (search "-FILTER" string-upcase)
		   (or (find-symbol string-upcase :graph)
		       (find-symbol string-upcase :app)
		       (error "~a is not a filter" *string*))
		   *string*))))
      (json:bind-custom-vars
	  (:beginning-of-string #'beginning :string-char #'parse-token
				:end-of-string #'end :string-scope '(*string*))
	(let ((form (json:decode-json-from-string json)))
	  (values (eval form) form))))))

(defmacro def-filter-route (path keyword)
  `(def-graph-route (:get ,path) (req res (filter-json))
     (list ,keyword (json-to-filter filter-json))))

(defroute (:get "/") (req res)
  (send-response res :headers '(:content-type "text/html; charset=utf-8")
		 :body (make-html
			 (:h1 "movie-graph")
			 (:p (:a :href "/clear/" "Clear") " "
			     (:input :id "add" :placeholder "Add")
			     (:input :id "update" :placeholder "Update")
			     (:input :id "filter-nodes" :placeholder "Filter nodes")
			     (:input :id "filter-edges" :placeholder "Filter edges")
			     (:div :id "state")
			     (:div :id "node-filter"))
			 (:object :id "graph" :data +graph-path+ :type "image/svg+xml"))))

(defroute (:get "/state/") (req res)
  (send-response res :headers '(:content-type "text/html; charset=utf-8")
		 :body (with-html-string
			 (:p (:b "graph-classes: ") (str *graph-classes*))
			     (:p (:b "node-filter: ") (esc (write-to-string *node-filter*)))
			     (:p (:b "edge-filter: ") (esc (write-to-string *edge-filter*))))))

(defroute (:get "/graph/nodes/") (req res)
  (send-json-response res (graph:vertices (app:current-graph))))

(defroute (:get "/graph/edges/") (req res)
  (send-json-response res (graph:edges (app:current-graph))))

(def-graph-route (:get "/clear/") (req res)
  (app:clear-graph)
  nil)

(def-graph-route (:get "/add/(.*)") (req res (movies))
  (apply #'app:add-movies (split-sequence #\/ movies :remove-empty-subseqs t))
  nil)

(def-graph-route (:get "/update/(.*)") (req res (classes))
  (list :graph-classes (mapcar (lambda (class) (intern (string-upcase class) :app))
			       (split-sequence #\/ classes :remove-empty-subseqs t))))

(def-filter-route "/filter/node/(.*)" :node-filter)
(def-filter-route "/filter/edge/(.*)" :edge-filter)

(defsearchroute "search" imdb:do-search imdb:id-class)
(defsearchroute "inverse-search" imdb:inverse-search imdb:inverse-id-class)

(def-directory-route "/assets" "./assets")

(defroute (:* ".+" :priority -1) (req res)
  (send-response res :body "Page not found." :status 404))

(defun serve (&optional (port 3000))
  (update-graph)
  (as:with-event-loop ()
    (let* ((listener (make-instance 'listener :bind "127.0.0.1" :port port))
	   (server (start-server listener)))
      (as:signal-handler 2 (lambda (sig)
			     (declare (ignore sig))
			     (as:free-signal-handler 2)
			     (as:close-tcp-server server))))))
