(in-package #:wsf)

(defgeneric site-port (site))
(defgeneric site-host (site))
(defgeneric site-docroot (site))
(defgeneric site-controller (site))

(defgeneric ajax-actions (site))

(defun site-acceptor (site)
  (acceptor (site-port site)))

(defun open-site (site)
  (pushnew site (acceptor-sites (site-acceptor site)))
  (setf (acceptor-request-dispatcher (site-acceptor site))
        (lambda (request)
          (respond site request))))

(defun close-site (site)
  (let ((acceptor (site-acceptor site)))
    (when (< 0 (length (setf (acceptor-sites acceptor)
                             (delete site (acceptor-sites acceptor)))))
      (release-port (site-port site)))))

(defclass site (kgb::system)
  ((port :initarg :port :accessor site-port :initform (error "Need port!"))
   (host :initarg :host :accessor site-host :initform (error "Need host!"))
   (docroot :initarg :docroot :accessor site-docroot :initform (user-homedir-pathname))
   (controller :reader site-controller :initform (make-instance 'controller))
   (ajax-actions :accessor ajax-actions :initform (make-hash-table))))

(defmethod initialize-instance :after ((site site) &key)
  (set-ajax-route site))

(defmethod initialize-instance :around ((site site) &key)
  (call-next-method)
  (open-site site))
