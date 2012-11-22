;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

;; string

(defun string-request (server request)
  (with-http-acceptor server
    (uri-http-request request)))

(defmethod respond :around ((server http-server) (uri string))
  (respond server (string-request server uri)))

;; request

(defmacro with-http-reply (&body body)
  `(if (within-http-request?)
       (if (within-headless-reply?)
           (progn ,@body)
           (with-http-session ,@body))
       (with-headless-reply ,@body)))

(defun throw-response (response)
  (throw 'response response))

(defmethod respond :around ((server http-server) request)
  (with-server-request (server request)
    (with-http-acceptor server
      (with-http-reply
        (awith (catch 'response
                 (with-trivial-handlers
                     (call-next-method)))
          (send (typecase it
                  (response it)
                  (string (string-response it))
                  (t (default-response server request)))))))))
