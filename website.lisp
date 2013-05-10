;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defclass website (docroot-server parse-server router http-server) ())

(defparameter server-charset :utf-8)

;(defmacro catch-server-conditions (&body body)
;  `(handler-case (progn ,@body)
;     (hunchentoot-simple-warning ()
;       (print "it's a hunchentoot warning!"))))

(defmethod respond :around ((server website) request)
  (let ((*hunchentoot-default-external-format* (charset-instance server-charset)))
    ;(catch-server-conditions
    (call-next-method)))

(defparameter error-pages-folder-pathname "view/page/error")

(defmethod initialize-instance :after ((server website) &key)
  (setf (acceptor-error-template-directory (server-acceptor server))
        (from-docroot server error-pages-folder-pathname "/")))

(defmacro defsite (name (&rest superclasses) &rest args)
  `(progn

     (defclass ,name (,@superclasses wsf:website) ())

     (defvar ,name
       (make-instance ',name ,@args))

     (setf *server* (symbol-value ',name))

     (define-symbol-macro docroot
         (from-docroot (symbol-value ',name)))))
