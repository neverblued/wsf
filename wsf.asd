(defpackage #:wsf-system
  (:use
     #:common-lisp
     #:asdf
     ))

(in-package #:wsf-system)

(defsystem "wsf"
  :description "Web Site Framework"
  :version "0.1"
  :author "Demetrius Conde <condemetrius@gmail.com>"
  :licence "Public Domain"
  :depends-on (#:osicat #:hunchentoot #:dc-bin #:jsun #:kgb)
  :serial t
  :components ((:file "package")
               (:file "http")
               (:file "files")
               (:file "responses")
               (:file "kgb")
               (:file "respond")
               (:file "controller")
               (:file "ajax")
               (:file "site")
               (:file "debug")))
