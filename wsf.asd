(defpackage #:wsf-system
  (:use
     #:common-lisp
     #:asdf
     ))

(in-package #:wsf-system)

(defsystem "wsf"
  :description "Web Site Framework"
  :version "0.2"
  :author "Demetrius Conde <condemetrius@gmail.com>"
  :licence "Public Domain"
  :depends-on (#:hunchentoot #:dc-bin #:jsun #:kgb) ; #:osicat
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
