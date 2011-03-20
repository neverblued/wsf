(defpackage #:wsf-system
  (:use #:common-lisp #:asdf))

(in-package #:wsf-system)

(defsystem "wsf"
  :description "Web Site Framework"
  :version "0.2"
  :author "Demetrius Conde <condemetrius@gmail.com>"
  :depends-on (#:cl-blackjack #:jsun #:hunchentoot #:kgb)
  :serial t
  :components ((:file "container")
               (:file "selector")
               (:file "package")
               (:file "http")
               (:file "files")
               (:file "responses")
               (:file "kgb")
               (:file "respond")
               (:file "controller")
               (:file "ajax")
               (:file "site")
               (:file "debug")))
