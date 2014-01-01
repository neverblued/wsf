;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defun slime-debug? ()
  (and slime-debug-conditions (within-headless-reply?)))

;(let ((hunchentoot::*hunchentoot-stream* *debug-io*))

(defmacro with-trivial-handlers (&body body)
  `(handler-case (progn ,@body)
     ((or warning error wsf-condition)
         (condition)
       (if (slime-debug?)
           (invoke-debugger condition)
           (awith (failure-response *server* *request* condition)
             (throw-response it))))))
