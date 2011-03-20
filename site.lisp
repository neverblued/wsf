(in-package #:wsf)

(defgeneric open-site (site))
(defgeneric close-site (site))

(defgeneric site-port (site))
(defgeneric site-acceptor (site))

(defmethod site-acceptor (site)
  (port-acceptor (site-port site)))

(defmethod open-site :before (site)
  (stop&start (site-acceptor site)))

(defmethod open-site (site)
  (setf (acceptor-request-dispatcher (site-acceptor site))
        (lambda (request)
          (respond site request))))

(defmethod close-site (site)
  (release-port (site-port site)))

(defclass site ()
  ((port :initform (next-free-port) :initarg :port :accessor site-port)
   (docroot :initform (user-homedir-pathname) :initarg :docroot :accessor site-docroot)
   (controller :initform (make-instance 'controller) :reader site-controller)
   (ajax-actions :initform (make-hash-table) :accessor ajax-actions)))

(defmethod initialize-instance :after ((site site) &key)
  (set-ajax-route site))

(defmethod initialize-instance :around ((site site) &key)
  (call-next-method)
  (open-site site))
