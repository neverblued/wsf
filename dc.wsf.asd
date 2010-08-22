(defpackage #:dc.wsf-system
  (:use
     #:common-lisp
     #:asdf
     )
  )

(in-package #:dc.wsf-system)

(defsystem "dc.wsf"
  :description "DC Web Site Framework."
  :version "0.1"
  :author "Demetrius Conde <condemetrius@gmail.com>"
  :licence "Public Domain"
  :depends-on (#:hunchentoot #:cl-ppcre #:dc.utils)
  :serial t
  :components (
               (:file "dc.wsf")
               (:file "service")
               (:file "site")
               (:file "response")
               (:file "view")
               ))
