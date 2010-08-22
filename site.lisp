(in-package #:dc.wsf)

;;; ~~~~~~~~
;;; Acceptor
;;; ~~~~~~~~

(defparameter *first-port* 8080)

(defvar *acceptors* (make-hash-table))

(defclass site-acceptor (hunchentoot::acceptor)
  (
   (listening? :initform nil
               :reader listening?
               )
   ))

(defmethod start ((acceptor site-acceptor))
  (unless (listening? acceptor)
    (call-next-method)))

(defmethod start :after ((acceptor site-acceptor))
  (setf (slot-value acceptor 'listening?) t))

(defmethod stop ((acceptor site-acceptor))
  (when (listening? acceptor)
    (call-next-method)))

(defmethod stop :after ((acceptor site-acceptor))
  (setf (slot-value acceptor 'listening?) nil))

(defmethod stop&start ((acceptor site-acceptor))
  (stop acceptor)
  (start acceptor))

(macrolet ((acceptor (port)
             `(or (gethash ,port *acceptors*))))

  (defun port->acceptor (port)
    (or (acceptor port)
        (setf (acceptor port)
              (make-instance 'site-acceptor :port port))))

  (defun port-free? (port)
    (not (acceptor port)))

  )

(defun release-port (port)
  (remhash port *acceptors*))

(defun next-free-port ()
  (do ( ; 1 - vars
       (port *first-port* (1+ port))
       )
      ( ; 2
       (port-free? port) ; examine
       port ; return
       )))

;;; ~~~~
;;; Site
;;; ~~~~

(defclass site (server)
  (
   (id :initform nil
       :initarg :id
       :accessor id
       )
   (port :initform nil
         :initarg :port
         :accessor port
         )
   (docroot :initform nil
            :initarg :docroot
            :accessor docroot
            )
   (package :initform nil
            :initarg :package
            :reader site->package
            )
   ))

(defmacro define-site (name-symbol &rest plist)
  `(progn
     (defparameter ,name-symbol (make-instance 'site ,@plist))
     (port ,name-symbol)))

(defmacro with-site-docroot (site &body body)
  `(macrolet ((docroot+ (file-path)
                `(pathname (format nil "~a~a" (docroot ,',site) ,file-path))))
     ,@body))

(defmethod site-pathname ((site site) file-path)
  (pathname (format nil "~a~a" (docroot site) file-path)))

(defmethod site->acceptor ((site site))
  (port->acceptor (port site)))

(defmethod dispatch ((site site))
  (setf (hunchentoot::acceptor-request-dispatcher (site->acceptor site))
        (dispatcher site)))

(defmethod initialize-instance :after ((site site) &key)
  (setf (slot-value site 'package)
        *package*)
  (unless (port site)
    (setf (port site)
          (next-free-port)))
  (stop&start (site->acceptor site))
  (dispatch site))

(defmethod build-implemented? ((site site))
  (find-method #'build () `((eql ,site)) nil))

(defmethod build :around ((site site) &key)
  (let ((*site* site))
    (declare (special *site*))
    (call-next-method)))

(defparameter *request* nil)
(defparameter *response* nil)

(defmethod respond ((site site) &optional (request nil))
  (let ((*request* (if (boundp 'hunchentoot::*request*)
                       hunchentoot::*request*
                       request))
        (*response* nil))
    (if (build-implemented? site)
        (build site)
        (setf *response* (response[not-implemented] site)))
    (send (if (is-response? *response*)
              *response*
              (response[not-found] site)
              ))
    ))

;;; ~~~~~~~~~~~~~
;;; Site Response
;;; ~~~~~~~~~~~~~

(defclass site-response (response)
  (
   (content-type :initform "text/plain"
                 :initarg :content-type
                 :accessor content-type
                 )
   (content :initform "Hello, world! I am a WSF::SITE-RESPONSE template content."
            )
   (charset :initform :utf-8
            :initarg :charset
            :accessor charset
            )
   ))

(defmethod format-content-type ((response site-response))
  (format nil "~a; charset=~a" (content-type response) (charset-string (charset response))))

(defmethod send :before ((response site-response) &key)
  (when (boundp 'hunchentoot::*reply*)
    (setf (content-type* hunchentoot::*reply*)
          (format-content-type response))
    (setf hunchentoot::*hunchentoot-default-external-format*
          (charset-instance (charset response)))
    ))
