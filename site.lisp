(in-package #:wsf)

(defgeneric site-domain (site))
(defgeneric site-port (site))
(defgeneric site-docroot (site))
(defgeneric site-controller (site))

(defgeneric ajax-actions (site))

(defclass site (kgb::system)
  ((domain :initarg :domain :accessor site-domain :initform nil)
   (port :initarg :port :accessor site-port :initform default-port)
   (docroot :initarg :docroot :accessor site-docroot :initform (user-homedir-pathname))
   (controller :reader site-controller :initform (make-instance 'controller))
   (ajax-actions :accessor ajax-actions :initform (make-hash-table))))

(defmethod initialize-instance :after ((site site) &key)
  (set-ajax-route site)
  (start site))

(defmethod start ((site site))
  (start (site-acceptor site)))

(defmethod stop ((site site))
  (stop (site-acceptor site)))

(defun online? (site)
  (accepting? (site-acceptor site)))
