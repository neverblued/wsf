(in-package #:wsf)

;;; route

(defgeneric route-controller (route))
(defgeneric route-name (route))
(defgeneric route-clause (route))
(defgeneric route-action (route))
(defgeneric route-encoder (route))
(defgeneric route-decoder (route))

(defgeneric route-args (route request))

(defmethod route-args (route request)
  (let ((decoder (route-decoder route))
        (args (list :*request* request)))
    (if decoder
        (append args (funcall decoder request))
        args)))

(defclass route ()
  ((controller :initarg :controller :accessor route-controller)
   (name :initarg :name :accessor route-name)
   (clause :initarg :clause :accessor route-clause)
   (action :initarg :action :accessor route-action)
   (encoder :initarg :encoder :accessor route-encoder)
   (decoder :initarg :decoder :accessor route-decoder)))

;;; controller

(defgeneric routes (controller))

(defclass controller ()
  ((routes :accessor routes :initform nil)))

;;; selection

(defvar *route*)

(defun route (controller &optional request)
  (let ((route (car (remove-if-not (lambda (route)
                                     (funcall (route-clause route) request))
                                   (routes controller)))))
    (unless route
      (error (make-condition 'route-not-found :controller controller :request request)))
    (let ((action (route-action route)))
      (unless (functionp action)
        (error (make-condition 'invalid-action :route route :expected-type 'function :datum action)))
      (let ((*route* route))
        (apply action (route-args route request))))))

;;; link

(defgeneric link (controller route-name &optional args)
  (:documentation "Make a link."))

(defparameter broken-link "/404-broken-link/")

(defmethod link (controller route-name &optional (args nil))
  (aif (find route-name (routes controller) :key #'route-name :test #'equal)
       (careful-apply (route-encoder it) args)
       broken-link))

;; setup

(defmacro set-route (controller name &key args link clause params action)
  `(progn (setf (routes ,controller)
                (delete ,name (routes ,controller) :key #'route-name :test #'equal))
          (push (make-instance 'route
                               :name ,name
                               :encoder (lambda (&key ,@args)
                                          ,(when args `(declare (ignorable ,@args)))
                                          ,link)
                               :clause (lambda (*request*)
                                         ,(aif clause it `(string= (request-uri *request*) ,link)))
                               :decoder (lambda (*request*)
                                          ,(awhen params it))
                               :action (lambda (&key *request* ,@args)
                                         ,(when args `(declare (ignorable ,@args)))
                                         ,action))
                (routes ,controller))))

(defun unset-route (controller name)
  (setf (routes controller)
        (delete name (routes controller) :key #'route-name :test #'equal)))
