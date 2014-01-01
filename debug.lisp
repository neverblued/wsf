;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

;; debug

(defparameter last-request nil)

(defparameter last-requests nil)

(defparameter last-requests-length 5)

(defun log-request (request)
  (setf last-request request)
  (push-trim last-request last-requests
             :limit last-requests-length))

(defmethod respond :before ((server http-server) (request request))
  (log-request request))

(defun last-request-uris ()
  (mapcar #'request-uri last-requests))

(defun test-last-requests (server)
  (mapcar (lambda (request)
            (respond server request))
          last-requests))
