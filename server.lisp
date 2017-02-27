(in-package :server)

(load-plugins)

(defmacro defroute (options (bind-request bind-response &optional bind-args) &body body)
  `(wookie:defroute ,options (,bind-request ,bind-response args)
     (destructuring-bind ,bind-args args ,@body)))

(defroute (:get "/") (req res)
  (send-response res :body "Welcome!"))

(defmacro defsearchroute (path function id-class-function)
  `(defroute (:get ,(format nil "/~a/(.+)/(.+)" path)) (req res (list-string id-string))
     (let* ((list (make-list-instance list-string))
	    (id-object (make-instance (,id-class-function list) :string id-string))
	    (results (,function list id-object)))
       (setf (getf (response-headers res) :content-type) "application/json")
       (send-response res :body (json:encode-json-to-string results)))))

(defsearchroute "search" do-search id-class)
(defsearchroute "inverse-search" inverse-search inverse-id-class)

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
