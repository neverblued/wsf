;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(defpackage #:wsf
  (:use #:cl #:cl-ppcre #:hunchentoot #:iterate #:alexandria
        #:blackjack)
  (:shadow #:acceptor #:content-type)
  (:export
                                        ; api
   #:defsite #:defroute #:defroute-ajax #:respond-page
                                        ; website
   #:website

   ;; basic server
                                        ; interface
   #:respond #:default-response #:failure-response
   #:*server* #:*request* #:with-server
                                        ; lisp
   #:lisp-server #:server-system #:server-package #:with-server-package
   #:reload-system
                                        ; docroot
   #:docroot-server #:server-pathname #:from-docroot
   #:server-data-pathname-format
   #:docroot #:docroot/ #:text-docroot/ #:list-docroot/
   #:define-file-datum #:load-file-datum #:defile
                                        ; parse
   #:parse-server #:server-parsers #:ranked-parsers #:parse
   #:regex-parser

   ;; web server
                                        ; responses
   #:text-response #:json-response #:html-response
                                        ; http
   #:http-server #:server-domain #:server-port #:stop&start #:online?
   #:throw-response #:robot? #:assert-session
   #:within-headless-reply?
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
   #:ajax-action #:ajax-parameters #:ajax-parameter

   #:ajax-boolean
   #:ajax-keyword
   #:ajax-string
   #:ajax-null-string
   #:ajax-symbol
   #:ajax-time
   #:ajax-value

   ))
