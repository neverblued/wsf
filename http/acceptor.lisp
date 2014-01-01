;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

;; acceptors

(defvar acceptors nil)

(defun kill-acceptor (acceptor)
  (stop acceptor)
  (setf acceptors (delete acceptor acceptors)))

(defun acceptors (&key port domain)
  (remove-if-not (lambda (acceptor)
                   (and (or (null port)
                            (equal port (acceptor-port acceptor)))
                        (or (null domain)
                            (equal domain (acceptor-address acceptor)))))
                 acceptors))

(defun (setf acceptors) (new-list)
  (mapcar #'kill-acceptor (set-difference acceptors new-list))
  (setf acceptors new-list))

;; acceptor

(defgeneric accepting? (acceptor))
(defgeneric acceptor-server (acceptor))
(defgeneric (setf acceptor-server) (server acceptor))

(defclass acceptor (hunchentoot::acceptor)
  ((accepting? :reader accepting? :initform nil)
   (server :initarg :server :accessor acceptor-server :initform nil)))

(let ((handler #'handle-request))
  (fmakunbound 'handle-request)
  (defgeneric handle-request (acceptor request))
  (defmethod handle-request (acceptor request)
    (funcall handler acceptor request)))

(defmethod handle-request :around ((acceptor acceptor) request)
  (or (respond (acceptor-server acceptor) request)
      (call-next-method)))

(defmethod initialize-instance :after ((acceptor acceptor) &key)
  (pushnew acceptor acceptors)
  (start acceptor))

;(defmethod acceptor-status-message
;    ((acceptor acceptor) http-status-code
;     &rest properties &key &allow-other-keys)

;; stop & start

(defun stop&start (acceptor)
  (stop acceptor)
  (start acceptor))

(defmethod start :around ((acceptor acceptor))
  (unless (accepting? acceptor)
    (call-next-method)))

(defmethod start :after ((acceptor acceptor))
  (setf (slot-value acceptor 'accepting?) t)
  (signal 'server-on :server (acceptor-server acceptor)))

(defmethod stop :around ((acceptor acceptor) &key soft)
  (declare (ignore soft))
  (when (accepting? acceptor)
    (call-next-method)))

(defmethod stop :after ((acceptor acceptor) &key soft)
  (declare (ignore soft))
  (setf (slot-value acceptor 'accepting?) nil)
  (signal 'server-off :server (acceptor-server acceptor)))
