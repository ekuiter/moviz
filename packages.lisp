(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :split-sequence)
  (ql:quickload :cl-ppcre))

(defpackage :actor-list
  (:use :common-lisp :split-sequence)
  (:export :actor
	   :movie
	   :role
	   :actor-list
	   :do-search
	   :inverse-search
	   :name
	   :title
	   :billing
	   :actor=
	   :actor<
	   :movie=
	   :role=)
  (:shadow :file-length))

(defpackage :graph
  (:use :common-lisp)
  (:export :graph
	   :node
	   :edge
	   :add-node
	   :add-edge
	   :make-image
	   :show
	   :to-dot
	   :compare
	   :label
	   :vertices
	   :edges))

(defpackage :main
  (:use :common-lisp :actor-list :graph)
  (:export :main))

(load "actor-list")
(load "graph")
(load "main")