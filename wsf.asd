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
  :depends-on (#:osicat #:dc-bin #:hunchentight #:rought)
  :serial t
  :components ((:file "package")
               (:file "site")
               (:file "response")
               (:file "route")
               ))
