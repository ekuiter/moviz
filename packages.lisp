(in-package :cl-user)

(defpackage :actor-list
  (:use :common-lisp)
  (:export :actor
	   :movie
	   :actor-list
	   :do-search
	   :inverse-search
	   :name
	   :title
	   :actor=
	   :actor<
	   :movie=)
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