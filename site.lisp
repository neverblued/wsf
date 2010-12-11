(in-package #:wsf)

(defgeneric site-port (site)
  (:documentation "Site HTTP-port."))

(defgeneric site-docroot (site)
  (:documentation "Site docroot folder path."))

(defclass site (router)
  ((port          :initform nil :initarg :port    :accessor site-port)
   (docroot       :initform nil :initarg :docroot :accessor site-docroot)
   (container-key :initform #'route-name)))

(defgeneric controller (site controller-name))

(defmethod controller ((site site) controller-name)
  (route site controller-name))

(defun from-docroot (site file-path)
  (join (site-docroot site) "/" file-path))

;; Setup

(defmethod initialize-instance :after ((site site) &key)
  (unless (site-port site)
    (setf (site-port site) (next-free-port))))

(defgeneric site-acceptor (site)
  (:documentation "Get site acceptor."))

(defgeneric bind-accepting (site)
  (:documentation "Bind the site dispatcher function to the site's HTTP-acceptor."))

(defmethod initialize-instance :around ((site site) &key)
  (call-next-method)
  (stop&start (site-acceptor site))
  (bind-accepting site))

(defmethod site-acceptor ((site site))
  (listening-to (site-port site)))

(defmethod bind-accepting ((site site))
  (setf (hunchentoot::acceptor-request-dispatcher (site-acceptor site))
        (dispatcher site)))

;;; Respond

(defmethod respond ((site site) (request hunchentoot::request))
  (process site request))

;;; Debug

(defparameter last-request nil)

(defparameter last-requests nil)

(defmethod respond :before ((site site) (request hunchentoot::request))
  (setf last-request request)
  (setf last-requests
        (cons last-request
              (if (> 5 (length last-requests))
                  last-requests
                  (subseq last-requests 0 4)))))

(defun last-request-uris ()
  (mapcar #'hunchentoot::request-uri last-requests))

(defun test-last-requests (site)
  (mapcar (lambda (request)
            (process site request))
          last-requests))
