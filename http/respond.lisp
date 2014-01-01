;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

;; string

(defun string-request (server request)
  (with-http-acceptor server
    (uri-http-request request)))

(defmethod respond :around ((server http-server) (uri string))
  (respond server (string-request server uri)))

;; request

(defun within-http-request? ()
  (and (within-request-p)
       (boundp '*reply*)))

(defmacro with-http-reply (&body body)
  `(with-http-acceptor
     (if (within-http-request?)
         (if (within-headless-reply?)
             (progn ,@body)
             (with-http-session ,@body))
         (with-headless-reply ,@body))))

(defmacro assure-response ((server request) &body body)
  `(awith (catch 'response ,@body)
     (typecase it
       (response it)
       (string (string-response it))
       (t (default-response ,server ,request)))))

(defun throw-response (response)
  (throw 'response response))

(defmethod respond :around ((server http-server) request)
  (with-server-request (server request)
    (with-http-reply
      (awith (assure-response (server request)
               (with-trivial-handlers
                 (call-next-method)))
        (send it)))))
