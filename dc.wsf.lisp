;;; ~~~~~~~~~~~~~
;;; WSF Bootstrap
;;; ~~~~~~~~~~~~~

(defpackage #:dc.wsf
  (:use
     #:common-lisp
     #:hunchentoot
     #:cl-ppcre
     #:dc.utils
     )
  (:shadow
     #:*request*
     )
  (:export
     #:site
     #:define-site
     ;#:with-site-docroot
     #:site-pathname
     #:site->package
     #:port
     #:docroot
     #:*request*
     #:*response*
     #:site-response
     #:file-response
     #:text-response
     #:html-response
     )
  )