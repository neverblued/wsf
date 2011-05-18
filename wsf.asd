(defpackage #:wsf-system
  (:use #:common-lisp #:asdf))

(in-package #:wsf-system)

(defsystem "wsf"
  :description "Web Site Framework"
  :version "0.2"
  :author "Demetrius Conde <condemetrius@gmail.com>"
  :depends-on (#:blackjack #:jsun #:hunchentoot #:kgb #:postgrace)
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "http")
               (:file "responses")
               (:file "kgb")
               (:file "respond")
               (:file "controller")
               (:file "ajax")
               (:file "site")
               (:file "database")
               (:file "debug")
               (:file "goals")))
