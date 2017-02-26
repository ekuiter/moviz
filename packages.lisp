(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :split-sequence)
  (ql:quickload :cl-ppcre))

(defpackage :imdb
  (:use :common-lisp :split-sequence)
  (:export :actor :movie :role :actors-list :do-search :inverse-search
	   :name :readable-name :title :billing :actor= :actor< :movie=
	   :role-score :role= :role< :alternate-versions-list :make-list-instance
	   :alternate-versions :episode :notes :summary :summarize :summarize-all
	   :goofs-list :goofs :trivia-list :trivia :crazy-credits :crazy-credits-list
	   :soundtracks :soundtracks-list :quotes :quotes-list)
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
(load "notes-list")
(load "actors-list")
(load "graph")
(load "main")
(load "tests")