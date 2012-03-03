(in-package #:wsf)

(defgeneric site-domain (site))
(defgeneric site-port (site))
(defgeneric site-acceptor (site))
(defgeneric site-docroot (site))
(defgeneric site-database (site))
(defgeneric site-parsers (site))
(defgeneric ajax-actions (site))

(defun from-docroot (site relative-path)
  (join (site-docroot site) "/" relative-path))

(defclass site (router)
  ((system :initarg :system :accessor site-system :initform (error "no system"))
   (domain :initarg :domain :accessor site-domain :initform nil)
   (port :initarg :port :accessor site-port :initform default-port)
   (acceptor :accessor site-acceptor)
   (package :initform *package* :reader site-package)
   (database :initarg :database :accessor site-database :initform nil)
   (parsers :accessor site-parsers :initform nil)
   (ajax-actions :accessor ajax-actions :initform (make-hash-table))))

(defmethod site-docroot (site)
  (join "/" (apply #'join-by "/"
                   (rest
                    (pathname-directory
                     (asdf:system-definition-pathname
                      (asdf:find-system
                       (site-system site))))))))

(defmethod initialize-instance :after ((site site) &key)
  (setf (site-acceptor site) (fetch-acceptor site))
  (start site))

(defmethod start ((site site))
  (start (site-acceptor site)))

(defmethod stop ((site site))
  (stop (site-acceptor site)))

(defun online? (site)
  (accepting? (site-acceptor site)))
