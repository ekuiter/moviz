(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :split-sequence)
  (ql:quickload :cl-ppcre)
  (ql:quickload :wookie)
  (ql:quickload :cl-json)
  (ql:quickload :cl-who))

(defpackage :imdb
  (:use :common-lisp :split-sequence)
  (:export :actor :movie :role :actors-list :actresses-list :do-search :inverse-search
	   :name :readable-name :title :billing :actor= :actor< :movie=
	   :role-score :role= :role< :alternate-versions-list :make-list-instance
	   :alternate-versions :episode :notes :summary :summarize :summarize-all
	   :goofs-list :goofs :trivia-list :trivia :crazy-credits :crazy-credits-list
	   :soundtracks :soundtracks-list :quotes :quotes-list :record-class :id-class
	   :inverse-id-class)
  (:shadow :file-length))

(defpackage :graph
  (:use :common-lisp)
  (:export :graph :node :edge :node-1 :node-2 :add-node :add-edge :make-image
	   :make-image :show :to-dot :compare :label :vertices :edges :label-too-long-error))

(defpackage :app
  (:use :common-lisp :imdb :graph)
  (:export :to-dot :make-image :show :current-graph :clear-graph :add-movies :save-and-quit
	   :make-graph))

(defpackage :tests
  (:use :common-lisp :imdb)
  (:export :run-tests))

(defpackage :server
  (:use :common-lisp :split-sequence :wookie :wookie-plugin-export :cl-who)
  (:export :serve)
  (:shadow :defroute))

(load "imdb")
(load "notes-list")
(load "actors-list")
(load "graph")
(load "app")
(load "tests")
(load "server")
