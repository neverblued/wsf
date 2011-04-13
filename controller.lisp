(in-package #:wsf)

;;; clause

(defmacro make-clause (get test example)
  `(list #',get #',test ,example))

(defun clause-get (clause)
  (first clause))

(defun clause-test (clause)
  (second clause))

(defun clause-example (clause)
  (third clause))

(defun clause-match? (clause request)
  (funcall (clause-test clause)
           (funcall (clause-get clause) request)
           (clause-example clause)))

;;; route

(defgeneric route (site request))
(defgeneric route-name (route))
(defgeneric route-clause (route))
(defgeneric route-action (route))
(defgeneric route-encoder (route))
(defgeneric route-decoder (route))
(defgeneric route-args (route request))

(defmethod route (site request)
  (select (site-controller site) request))

(defmethod route-args (route request)
  (let ((decoder (route-decoder route))
        (default-args (list :*request* request)))
    (if decoder
        (append default-args (funcall decoder request))
        default-args)))

(defclass route (containable)
  ((route-name :initarg :name :accessor route-name)
   (route-clause :initarg :clause :accessor route-clause)
   (route-action :initarg :action :accessor route-action)
   (route-encoder :initarg :encoder :accessor route-encoder)
   (route-decoder :initarg :decoder :accessor route-decoder)))

;;; controller

(defgeneric site-controller (site))

(defgeneric routes (controller))
(defgeneric (setf routes) (new-routes controller))

(defclass controller (container)
  ((container-key :initform #'route-name)))

(defmethod routes (controller)
  (container-list controller))

(defmethod (setf routes) (new-routes controller)
  (setf (container-list controller) new-routes))

;;; selection

(defmethod options ((controller controller))
  (routes controller))

(defmethod selection-predicate ((route route) (request request))
  (funcall (route-clause route) request))

(defvar *route*)

(defmethod succeed-selection ((controller controller) (request request) (route route))
  (let ((action (route-action route)))
    (if action
        (let ((*route* route))
          (apply action (route-args route request)))
        (fail-selection controller request))))

;;; link

(defgeneric link (site route-name &optional args)
  (:documentation "Make a link."))

(defmethod link (site route-name &optional (args nil))
  (let ((route (find-containing-key (site-controller site) route-name)))
    (if route
        (careful-apply (route-encoder route) args)
        "/404-broken-link/")))

;; setup

(defmacro set-route (site name &key args link clause params action)
  `(put-into (make-instance 'route
                            :name ,name
                            :encoder (lambda (&key ,@args)
                                       (declare (ignorable ,@args))
                                       ,link)
                            :clause (lambda (request)
                                      (let ((*request* request))
                                        (if (null ',clause)
                                            (string= (request-uri *request*) ,link)
                                            ,clause)))
                            :decoder (lambda (request)
                                       (let ((*request* request))
                                         ,params))
                            :action (lambda (&key *request* ,@args)
                                      (declare (ignorable ,@args))
                                      ,action)
                            )
             (site-controller ,site)))
