(in-package #:wsf)

(defgeneric site-port (site)
  (:documentation "Site HTTP-port."))

(defgeneric site-docroot (site)
  (:documentation "Site docroot folder path."))

(defgeneric site-router (site)
  (:documentation "Site router."))

(defgeneric site-ajax-action (site action-name)
  (:documentation "Site AJAX action (by name)."))

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
   (ajax-actions :initform (make-hash-table)
                 )
   ))

;; docroot

(defun from-docroot (site file-path)
  (join (site-docroot site) "/" file-path))

;; port

(defmethod initialize-instance :after ((site site) &key)
  (unless (site-port site)
    (setf (site-port site) (next-free-port))))

;; acceptor

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

;; controller

(defmethod site-controller ((site site) controller-name)
  (route site controller-name))

(defgeneric site-controllers (site))

(defmethod site-controllers ((site site))
  (rought::routes site))

;;; respond

(defparameter *response* nil)

(defgeneric response-404 (site request))

(defmethod respond :around ((site site) (request hunchentoot::request))
  (let (*response*)
    (declare (special *response*))
    (call-next-method)
    (send (if (typep *response* 'response)
              *response*
              (respond-failure site request)))))

(defmethod respond ((site site) (request hunchentoot::request))
  (process site request))

(defmethod response-404 ((site site) request)
  (declare (ignore site request))
  (make-instance 'text-response
                 :content (join "Unfortunately, the website has no"
                                " appropriate content for your request"
                                " (404)."
                                )
                 ))

;; ajax

(macrolet ((ajax-action (site action-name)
             `(gethash ,action-name (slot-value ,site 'ajax-actions))))

  (defmethod site-ajax-action ((site site) action-name)
    (ajax-action site action-name))

  (defmethod (setf site-ajax-action) (new-action (site site) action-name)
    (setf (ajax-action site action-name) new-action)))

(defmacro set-ajax (site action-name (&rest action-args) &body action-body)
  `(setf (site-ajax-action ,site ,action-name)
         (lambda (&key ,@action-args)
           (declare (ignorable ,@action-args))
           ,@action-body)))

(defun response-ajax (site action-name action-args)
  (let ((action (site-ajax-action site action-name)))
    (if action
        (apply action action-args)
        (list :ajax-error (list "unknown_action_name" action-name)))))

;;; debug

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
