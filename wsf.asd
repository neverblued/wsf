;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(defpackage #:wsf-system
  (:use #:common-lisp #:asdf))

(in-package #:wsf-system)

(defsystem #:wsf
  :description "Web Site Framework"
  :version "0.3"
  :author "Дмитрий Пинский <demetrius@neverblued.info>"
  :licence "LLGPL"
  :depends-on (#:hunchentoot #:alexandria #:iterate
               #:blackjack #:jsun)
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "responses")
               (:file "server")
               (:file "lisp")
               (:file "docroot")
               (:file "parse")
               (:file "http")
               (:file "pookies")
               (:module "routing"
                        :components ((:file "route")
                                     (:file "scope")
                                     (:file "router")
                                     (:file "operation")
                                     (:file "get-parameters")
                                     (:file "link")
                                     (:file "setup")))
               (:file "ajax")
               (:file "website")))
