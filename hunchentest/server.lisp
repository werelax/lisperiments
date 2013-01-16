(load #P"/Users/elias/quicklisp/setup.lisp")

(ql:quickload :hunchentoot)

(defpackage :hunchentest
  (:use :cl :hunchentoot)
  (:export :start-server))

(in-package :hunchentest)

(defun start-server ()
  )
