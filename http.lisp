(in-package #:wsf)

;; mode

(defclass acceptor (hunchentoot::acceptor)
  (
   (accepting? :initform nil
               :accessor accepting?
               )
   ))

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

(defvar *acceptors* (make-hash-table))

(macrolet ((acceptor (port)
             `(gethash ,port *acceptors*)))

  (defun port-acceptor (port)
    (or (acceptor port)
        (setf (acceptor port)
              (make-instance 'acceptor :port port))))

  (defun port-free? (port)
    (null (acceptor port)))

  (defgeneric release-port (port))

  (defmethod release-port :before (port)
    (let ((acceptor (acceptor port)))
      (when acceptor
        (stop acceptor)))))

(defmethod release-port (port)
  (remhash port *acceptors*))

(defparameter *first-port* 8123)

(defun next-free-port ()
  (do ((port *first-port* (1+ port)))
      ((port-free? port)
       port)))

;; site

(defgeneric site-port (site)
  (:documentation "Site HTTP-port."))

(defun site-acceptor (site)
  "Acceptor of the SITE."
  (port-acceptor (site-port site)))
