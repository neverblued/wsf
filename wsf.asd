;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(defpackage #:wsf-system
  (:use #:common-lisp #:asdf))

(in-package #:wsf-system)

(defsystem #:wsf
  :description "Web Site Framework"
  :version "0.7"
  :author "Дмитрий Пинский <demetrius@neverblued.info>"
  :licence "LLGPL"
  :depends-on (#:hunchentoot #:alexandria #:iterate
               #:blackjack #:jsun)
  :serial t
  :components ((:file "package")
               (:file "tools")
               (:file "config")
               (:file "charset")

               (:file "conditions")
               (:file "response")
               (:file "server")
               (:file "lisp")
               (:file "docroot-server")
               (:file "docroot-data")
               (:file "parse")

               (:module "http"
                        :components ((:file "config")
                                     (:file "acceptor")
                                     (:file "server")
                                     (:file "headless")
                                     (:file "response")
                                     (:file "session")
                                     (:file "debug")
                                     (:file "respond")
                                     (:file "robot")))

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
               (:file "website")

               (:file "debug")))
