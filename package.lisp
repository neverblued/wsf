(defpackage #:wsf
  (:use #:common-lisp #:osicat
        #:dc-bin #:hunchentight #:rought)
  (:shadow #:*request*)
  (:export #:site #:site-port #:site-docroot #:from-docroot
           #:mount #:site-route #:mount-route #:*response*
           #:file-response #:text-response #:html-response
           #:dongle-response #:empty-response #:test-response
           ))