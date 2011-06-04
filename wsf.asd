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
               (:file "utils")
               (:file "conditions")
               (:file "http")
               (:file "responses")
               (:file "database")
               (:file "kgb")
               (:file "route")
               (:file "respond")
               (:file "ajax")
               (:file "site")
               (:file "debug")
               (:file "goals")))
