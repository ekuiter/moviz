(in-package :server)

(defparameter +graph-path+ "assets/graph.svg")
(defparameter +export-path+ "assets/graph.png")
(defvar *graph-classes* nil)
(defvar *node-filter* (graph:all-filter))
(defvar *edge-filter* (graph:all-filter))
(defvar *destructive-operation-running* nil)
(defvar *destructive-operation-progress* "")
(defvar *destructive-operation-process* nil)

(load-plugins)
(setf (cl-who:html-mode) :html5)

(defmacro defroute (options (bind-request bind-response &optional bind-args) &body body)
  `(wookie:defroute ,options (,bind-request ,bind-response args)
     (destructuring-bind ,bind-args args ,@body)))

(defun send-json-response (res json)
  (send-response res :headers '(:content-type "application/json")
		 :body (json:encode-json-to-string json)))

(defmacro def-search-route (path function id-class-function)
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
  (let ((stylesheets (list "vendor/jquery-ui.min" "vendor/jquery.qtip.min" "app"))
	(scripts (list "vendor/jquery.min" "vendor/jquery-ui.min" "vendor/svg-pan-zoom.min"
		       "vendor/jquery.qtip.min" "helpers" "server" "filter" "node-filter"
		       "edge-filter" "graph-classes" "sidebar" "add-movies" "progress"
		       "debug" "graph" "app")))
    `(with-html-string
       (:html (:head (:title "moviz")
		     (:meta :charset "utf-8")
		     (:meta :name "viewport" :content "width=device-width, initial-scale=1")
		     (:link :rel "icon" :href "assets/images/favicon.ico")
		     ,@(mapcar (lambda (stylesheet)
				 `(:link :rel "stylesheet" :href
					 ,(format nil "assets/~a.css" stylesheet)))
			       stylesheets)
		     (:script "if (typeof module === 'object')"
			      "{ window.module = module; module = undefined; }")
		     ,@(mapcar (lambda (script)
				 `(:script :src ,(format nil "assets/~a.js" script)))
			       scripts)
		     (:script "if (window.module) module = window.module;"))
	      (:body ,@body)))))

(defun make-graph (graph-classes node-filter edge-filter)
  (let* ((graph (eval `(app:make-graph (app:current-graph) ,@graph-classes)))
	 (graph (graph:filter-nodes graph node-filter))
	 (graph (graph:filter-edges graph edge-filter)))
    graph))

(defun update-graph (&key (graph-classes *graph-classes*) (node-filter *node-filter*)
		       (edge-filter *edge-filter*) (body ""))
  (let ((graph (make-graph graph-classes node-filter edge-filter)))
    (setf *graph-classes* graph-classes *node-filter* node-filter *edge-filter* edge-filter)
    (app:make-image graph +graph-path+ :format "svg"))
  body)

(defmacro def-graph-route (options (bind-request bind-response &optional bind-args) &body body)
  `(defroute ,options (,bind-request ,bind-response ,bind-args)
     (let ((body (apply #'update-graph (progn ,@body))))
       (unless (eql body :none)
	 (send-response res :body body)))))

(defmacro def-destructive-graph-route (options (bind-request bind-response &optional bind-args)
				       &body body)
  `(def-graph-route ,options (,bind-request ,bind-response ,bind-args)
     (when *destructive-operation-running*
       (error "a destructive operation is already running"))
     ,@body))

(defmacro def-filter-route (path keyword)
  `(def-graph-route (:get ,path) (req res (filter-json))
     (list ,keyword (eval (json-helpers:to-form filter-json :to-symbol "-FILTER"
						:packages '(:graph :app))))))

(defun calculate-progress (progress-string)
  (let ((percent (parse-integer
		  (first (last (cl-ppcre:all-matches-as-strings "\\d{2}|\\d" progress-string)))))
	(actresses (search "actresses" progress-string)))
    (+ (if actresses 50 0) (/ percent 2))))

(defun date-string ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defroute (:get "/") (req res)
  (let ((body (make-html
		(:div :id "wrapper"
		      (:div :id "sidebar-pad")
		      (:object :id "graph" :type "image/svg+xml")
		      (:div :id "empty-graph" (:p "Nothing here.")
			    (:p (:a :href "#" :class "add" "Add some movies")
				(:span :class "show" " or "
				       (:a :href "#" :class "all" "show them all")) ".")))
		(:div :id "overlay")
		(:div :id "sidebar"
		      (:div :class "pad" "moviz")
		      (:ul :id "menu"
			   (:li :class "ui-widget-header" (:div "moviz"))
			   (:li :id "collapse"
				(:div (:span :class "ui-icon ui-icon-triangle-1-n")
				      (:span :class "text" "Hide")))
			   (:li :id "add"
				(:div (:span :class "ui-icon ui-icon-plusthick") "Add movies"))
			   (:li :id "clear"
				(:div (:span :class "ui-icon ui-icon-trash") "Clear graph"))
			   (:li :id "load"
				(:div (:span :class "ui-icon ui-icon-document") "Load graph"))
			   (:li :id "save"
				(:div (:span :class "ui-icon ui-icon-disk") "Save graph"))
			   (:li :id "export"
				(:div (:span :class "ui-icon ui-icon-image") "Export image"))
			   (:li :id "debug"
				(:div (:span :class "ui-icon ui-icon-wrench") "Debug"))
			   (:li :id "info"
				(:div (:span :class "ui-icon ui-icon-info") "Info")))
		      (:p (:b "Style"))
		      (:div :id "graph-classes")
		      (:p (:b "Actors"))
		      (:div :id "edge-filter")
		      (:p (:b "Movies"))
		      (:div :id "node-filter"
			    (:div :class "buttons"
				  (:button :class "all"
					   (:span :class "ui-icon ui-icon-circle-plus") " All")
				  (:button :class "none"
					   (:span :class "ui-icon ui-icon-circle-minus") " None"))
			    (:div :class "filters"))
		      (:p))
		(:div :id "info-dialog" :title "moviz v1.1"
		      (:p "moviz visualizes connections between movies using the IMDb.")
		      (:p "Visit on GitHub: " (:a :href "https://github.com/ekuiter/moviz"
						  :class "external" "ekuiter/moviz"))
		      (:div
		       (:img :src "assets/images/imdb.png" :width 40)
		       (:p "Information courtesy of "
			   (:a :href "http://www.imdb.com" :class "external" "IMDb")
			   ". Used with permission."))
		      (:div
		       (:img :src "assets/images/tmdb.svg" :width 40)
		       (:p "This product uses the TMDb API but is not endorsed or certified by "
			   (:a :href "http://www.themoviedb.org" :class "external" "TMDb") ".")))
		(:div :id "error-dialog" :title "Error")
		(:div :id "add-dialog" :title "Add movies"
		      (:p "Enter some movie titles:")
		      (:input))
		(:div :id "clear-dialog" :title "Clear graph"
		      (:p "Do you really want to remove all movies?"))
		(:div :id "load-dialog" :title "Load graph"
		      (:p "Enter the path of a graph file to load:")
		      (:input)
		      (:p :class "progress"))
		(:div :id "debug-dialog" :title "Debug"
		      (:p "Enter any Lisp form to evaluate on the server:")
		      (:input)
		      (:p :class "progress")
		      (:p :class "results"))
		(:div :id "progress-dialog" :title "Adding movies ..."
		      (:p :class "progress")))))
    (send-response res :headers '(:content-type "text/html; charset=utf-8") :body body)))

(defroute (:get "/graph/nodes/") (req res)
  (let ((app:*encoding-vertices* :summary))
    (send-json-response res (graph:vertices (app:current-graph)))))

(defroute (:get "/graph/edges/") (req res)
  (send-json-response res (graph:edges (app:current-graph))))

(def-destructive-graph-route (:get "/clear/") (req res)
  (app:clear-graph)
  nil)

(def-destructive-graph-route (:get "/add/(.*)") (req res (movies))
  (flet ((fn ()
	   (let ((stream (make-string-output-stream)))
	     (setf *destructive-operation-running* stream)
	     (unwind-protect (let ((*standard-output* *destructive-operation-running*))
			       (apply #'app:add-movies
				      (split-sequence #\/ movies :remove-empty-subseqs t)))
	       (setf *destructive-operation-running* nil
		     *destructive-operation-progress* ""
		     *destructive-operation-process* nil)
	       (close stream)))))
    (setf *destructive-operation-process* (ccl:process-run-function "add-movies" #'fn)))
  nil)

(defroute (:get "/abort/") (req res)
  (send-json-response res
		      (when *destructive-operation-process*
			(ccl:process-kill *destructive-operation-process*)
			t)))

(defroute (:get "/progress/") (req res)
  (let ((result
	 (if *destructive-operation-running*
	     (calculate-progress (setf *destructive-operation-progress*
				       (concatenate 'string *destructive-operation-progress*
						    (get-output-stream-string
						     *destructive-operation-running*))))
	     nil)))
    (send-json-response res result)))

(def-graph-route (:get "/update/(.*)") (req res (classes))
  (list :graph-classes (mapcar (lambda (class) (intern (string-upcase class) :app))
			       (split-sequence #\/ classes :remove-empty-subseqs t))))

(def-filter-route "/filter/node/(.*)" :node-filter)
(def-filter-route "/filter/edge/(.*)" :edge-filter)

(def-search-route "search" imdb:do-search imdb:id-class)
(def-search-route "inverse-search" imdb:inverse-search imdb:inverse-id-class)

(defroute (:get "/suggest/(.+)") (req res (title))
  (let* ((results (imdb:suggest (imdb:make-list-instance 'movies) title)))
    (send-json-response res results)))

(defroute (:get "/eval/(.+)") (req res (form))
  (send-json-response res (eval (read-from-string form))))

(defroute (:get "/graph/save/") (req res)
  (send-response res :headers (list :content-type "application/javascript"
				    :content-disposition
				    (format nil "attachment; filename=graph ~a.json"
					    (date-string)))
		 :body (app:encode-graph)))

(def-destructive-graph-route (:get "/graph/load/(.+)") (req res (file-name))
  (with-open-file (stream file-name)
    (let ((json (make-string (file-length stream))))
      (read-sequence json stream)
      (unless (eql (search "{\"prototype\":{\"lispClass\":\"movieGraph\"" json) 0)
	(error "not a valid graph file"))
      (app:restore-graph json)))
  nil)

(defun send-file (response path file-name)
  (let* ((headers (list
		   :content-type (wookie-plugin-core-directory-router::get-mime path)
		   :content-disposition (format nil "attachment; filename=~a" file-name)))
	 (stream (start-response response :headers headers))
	 (fstream (open path :element-type '(unsigned-byte 8)))
	 (fsize (file-length fstream))
	 (buffer (make-array (min fsize (* 1024 1024 8)) :element-type 'as:octet)))
    (labels ((finish ()
	       (close fstream)
	       (finish-response response))
	     (next ()
	       (let ((n (read-sequence buffer fstream)))
		 (if (zerop n)
		     (finish)
		     (progn
		       (write-sequence buffer stream :start 0 :end n)
		       (as:delay #'next))))))
      (next))))

(defroute (:get "/graph/export/") (req res)
  (let ((graph (make-graph *graph-classes* *node-filter* *edge-filter*)))
    (app:make-image graph +export-path+ :format "png"))
  (send-file res +export-path+ (format nil "graph ~a.png" (date-string))))

(def-directory-route "/assets" "./assets")

(defroute (:* ".+" :priority -1) (req res)
  (send-response res :body "Page not found." :status 404))

(defun serve (&key (address "127.0.0.1") (port 3000) debug)  
  (update-graph)
  (handler-bind
      ((cl-async:streamish-closed (lambda (c)
				    (declare (ignore c))
				    (invoke-restart 'cl-async::continue-event-loop)))
       (cl-async:socket-address-in-use (lambda (c)
					 (declare (ignore c))
					 (format t "port ~a is not available" port)
					 (return-from serve)))
       (warning (lambda (c) (unless debug (muffle-warning c)))))
    (as:with-event-loop ()
      (let* ((listener (make-instance 'listener :bind address :port port))
	     (server (start-server listener)))
	(format t "Serving on ~a:~a ..." address port)
	(as:signal-handler 2 (lambda (sig)
			       (declare (ignore sig))
			       (as:free-signal-handler 2)
			       (as:close-tcp-server server)))))))
