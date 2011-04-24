(in-package #:wsf)

(defclass acceptor (hunchentoot::acceptor)
  ((accepting? :accessor accepting? :initform nil)
   (sites :accessor acceptor-sites :initform nil)))

(defmethod initialize-instance :after ((acceptor acceptor) &key)
  (setf (acceptor-request-dispatcher acceptor)
        (lambda (request)
          (aif (find (host request) (acceptor-sites acceptor) :key #'site-host :test #'string=)
               (respond it request)
               (error 'unacceptable-host :host (host request)))))
  (start acceptor))

(defvar acceptors (make-hash-table))

(defparameter first-port 8666)

;; mode

(defmethod start :around ((acceptor acceptor))
  (unless (accepting? acceptor)
    (call-next-method)))

(defmethod start :after ((acceptor acceptor))
  (setf (accepting? acceptor) t))

(defmethod stop :around ((acceptor acceptor))
  (when (accepting? acceptor)
    (call-next-method)))

(defmethod stop :after ((acceptor acceptor))
  (setf (accepting? acceptor) nil))

(defun stop&start (acceptor)
  (stop acceptor)
  (start acceptor))

;; port

(defun port-free? (port)
  (null (gethash port acceptors)))

(defun next-free-port ()
  (do ((port first-port (1+ port)))
      ((port-free? port)
       port)))

(defgeneric release-port (port))

(defmethod release-port :before (port)
  (awhen (gethash port acceptors)
    (stop it)))

(defmethod release-port (port)
  (remhash port acceptors))

;; acceptors

(defun acceptor (&optional (port (next-free-port)))
  (let ((acceptor (gethash port acceptors)))
    (or acceptor
        (let ((acceptor (make-instance 'acceptor :port port)))
          (setf (gethash port acceptors) acceptor)
          acceptor))))
