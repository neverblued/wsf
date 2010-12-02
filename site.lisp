(in-package #:wsf)

(defclass site (router)
  (
   (port :initform nil
         :initarg :port
         :accessor site-port
         )
   (docroot :initform nil
            :initarg :docroot
            :accessor site-docroot
            )
   ))

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

(defgeneric from-docroot (site file-path)
  (:documentation "Get an absolute filepath"))

(defmethod from-docroot ((site site) file-path)
  (join (site-docroot site) "/" file-path))
