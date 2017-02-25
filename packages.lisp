(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :split-sequence)
  (ql:quickload :cl-ppcre))

(defpackage :imdb
  (:use :common-lisp :split-sequence)
  (:export :actor :movie :role :actors-list :do-search :inverse-search
	   :name :readable-name :title :billing :actor= :actor< :movie=
	   :role-score :role= :role< :alternate-versions-list :make-list-instance
	   :alternate-version :episode :notes :summary :summarize :summarize-all
	   :goofs-list :goof :trivia-list :trivia)
  (:shadow :file-length))

(defpackage :graph
  (:use :common-lisp)
  (:export :graph :node :edge :node-1 :node-2 :add-node :add-edge :make-image
	   :show :to-dot :compare :label :vertices :edges :label-too-long-error))

(defpackage :main
  (:use :common-lisp :imdb :graph))

(defpackage :tests
  (:use :common-lisp :imdb :graph :main)
  (:export :run-tests))

(load "imdb")
(load "movie-information-list")
(load "actors-list")
(load "alternate-versions-list")
(load "goofs-list")
(load "trivia-list")
(load "graph")
(load "main")
(load "tests")