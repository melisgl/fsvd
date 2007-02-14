;;;; -*- mode: Lisp -*-

(defpackage #:fsvd.system
  (:use #:cl #:asdf))

(in-package #:fsvd.system)

(defsystem #:fsvd
  :name "Simon Funk's quasi SVD"
  :description "This is a Common Lisp implementation of Simon Funk's
quasi svd as described at http://sifter.org/~simon/journal/20061211.html"
  :author "Gabor Melis"
  :version "0.0.2"
  :licence "MIT"
  :components ((:file "package")
               (:file "fsvd"))
  :serial t)
