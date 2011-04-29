(in-package #:wsf)

(defparameter default-port 8666)

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
(defgeneric acceptor-site (acceptor))
(defgeneric (setf acceptor-site) (site acceptor))

(defclass acceptor (hunchentoot::acceptor)
  ((accepting? :reader accepting? :initform nil)
   (site :initarg :site :accessor acceptor-site :initform nil)))

(defmethod initialize-instance :after ((acceptor acceptor) &key)
  (pushnew acceptor acceptors)
  (start acceptor))

(defmethod acceptor-request-dispatcher ((acceptor acceptor))
  (lambda (request)
    (respond (acceptor-site acceptor) request)))

(defun site-acceptor (site)
  (with-accessors ((port site-port) (domain site-domain)) site
    (or (let ((result (acceptors :domain domain :port port)))
          ;(awhen (rest result)           ; this sucks because of wildcart
          ;  (mapcar #'kill-acceptor it))
          (awhen (first result)
            (setf (acceptor-site it) site)
            it))
        (make-instance 'acceptor
                       :site site
                       :address domain
                       :port port))))

;; mode

(defmethod start :around ((acceptor acceptor))
  (unless (accepting? acceptor)
    (call-next-method)))

(defmethod start :after ((acceptor acceptor))
  (setf (slot-value acceptor 'accepting?) t)
  (signal 'site-on :site (acceptor-site acceptor)))

(defmethod stop :around ((acceptor acceptor))
  (when (accepting? acceptor)
    (call-next-method)))

(defmethod stop :after ((acceptor acceptor))
  (setf (slot-value acceptor 'accepting?) nil)
  (signal 'site-off :site (acceptor-site acceptor)))

(defun stop&start (acceptor)
  (stop acceptor)
  (start acceptor))
