(defpackage #:wsf
  (:use #:cl #:cl-blackjack #:hunchentoot #:kgb)
  (:shadow #:acceptor #:content-type)
  (:export #:site
           #:site-port #:stop&start ; http
           #:site-docroot #:from-docroot ; files
           #:*site* #:*request* #:*response* #:send ; respond
           #:default-response #:failure-response #:file-response #:text-response #:html-response ; responses
           #:set-route #:controller #:with-request #:link #:*route* #:route-name ; controller
           #:set-ajax-route #:set-ajax #:set-ajax-route #:respond-ajax #:ajax-win #:ajax-fail ; ajax
           #:set-auth-cookie #:kill-auth-cookie #:auth-cookie-life ; kgb
           #:goals #:achieve #:goal #:goal-alias #:goal-link #:goal-time #:goal-user #:goal-ip ; goals
           ))
