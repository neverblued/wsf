;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defgeneric server-parsers (parse-server))

(defclass parse-server (lisp-server)
  ((parsers :accessor server-parsers :initform nil)))

(defclass regex-parser ()
  ((regex :initarg :regex :accessor parser-regex)
   (patch :initarg :patch :accessor parser-patch)))

(defgeneric ranked-parsers (parse-server))

(defmethod ranked-parsers (server)
  (server-parsers server))

(defun parse (text &optional (server *server*))
  (let ((*package* (server-package server)))
    (iter (for parser in (ranked-parsers server))
          (with-accessors ((regex parser-regex)
                           (patch parser-patch))
              parser
            (setf text
                  (regex-replace-all regex text
                                     (awith patch
                                       (typecase it
                                         (string it)
                                         (function
                                          (lambda (source &rest patch-args)
                                           (awith (apply it patch-args)
                                             (or it source))))))
                                           :simple-calls t)))))
  text)
