;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(defpackage #:wsf
  (:use #:cl #:cl-ppcre #:hunchentoot #:iterate #:alexandria
        #:blackjack)
  (:shadow #:acceptor #:content-type)
  (:export
                                        ; website
   #:website

   ;; basic server
                                        ; interface
   #:respond #:default-response #:failure-response #:*server* #:*request*
                                        ; lisp
   #:lisp-server #:server-system #:server-package
                                        ; docroot
   #:docroot-server #:server-pathname #:from-docroot
                                        ; parse
   #:parse-server #:server-parsers #:ranked-parsers #:parse
   #:regex-parser

   ;; web server
                                        ; responses
   #:text-response #:html-response
                                        ; http
   #:http-server #:server-domain #:server-port #:stop&start #:online? #:throw-response #:yandex-bot?
                                        ; pookies
   #:pookies #:pookie-origin
                                        ; route
   #:router #:*router* #:with-router #:set-route #:unset-route
   #:route #:*route* #:*routes* #:route-name #:route-follow #:call-next-route
   #:link #:broken-link #:make-link
                                        ; links
   #:set-special-link #:another-link #:original-link
                                        ; ajax
   #:set-route-ajax #:ajax? #:ajax-win #:ajax-fail
   #:ajax-action #:ajax-parameters #:ajax-parameter #:ajax-string #:ajax-keyword #:ajax-value

   ;; extended server
                                        ; database
   #:database-server #:server-database
                                        ; kgb
   #:secure-server #:set-auth-cookie #:kill-auth-cookie #:auth-cookie-life

   ;; alpha shit
                                        ; goals
   ;#:goals #:achieve #:goal #:goal-alias #:goal-link #:goal-time #:goal-user #:goal-ip
   ))
