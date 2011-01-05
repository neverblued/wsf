(in-package #:wsf)

(defparameter last-request nil)

(defparameter last-requests nil)

(defmethod respond-site :before ((site site) (request request))
  (setf last-request request)
  (setf last-requests
        (cons last-request
              (if (> 5 (length last-requests))
                  last-requests
                  (subseq last-requests 0 4)))))

(defun last-request-uris ()
  (mapcar #'request-uri last-requests))

(defun test-last-requests (site)
  (mapcar (lambda (request)
            (route site request))
          last-requests))
