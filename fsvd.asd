;;;; -*- mode: Lisp -*-

(defpackage #:fsvd.system
  (:use #:cl #:asdf))

(in-package #:fsvd.system)

(defsystem #:fsvd
  :name "Simon Funk's quasi SVD"
  :author "Gabor Melis"
  :version "0.0.1"
  :components ((:file "package")
               (:file "fsvd"))
  :serial t)
