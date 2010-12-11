(in-package #:wsf)

;;; Clause

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

;;; Controller

(defgeneric clause (controller)
  (:documentation "Clause that should evaluate to T for matching the controller."))

(defgeneric action (controller)
  (:documentation "Action of the controller."))

(defgeneric encoder (controller)
  (:documentation "Controller encoder."))

(defgeneric decoder (controller)
  (:documentation "Controller decoder."))

(defclass controller (route)
  ((clause  :initarg :clause  :accessor clause  :initform nil)
   (action  :initarg :action  :accessor action  :initform nil)
   (decoder :initarg :decoder :accessor decoder :initform nil)
   (encoder :initarg :encoder :accessor encoder :initform nil)))

;;; Match

(defmethod match ((request hunchentoot::request) (controller controller))
  (flet ((integrate-test (clause request)
           `(macrolet ((with-request (getter tester example)
                         `(clause-match? (make-clause ,getter ,tester ,example) ,',request)))
              ,clause)))
    (macrolet ((test-against (clause request)
                 `(eval (integrate-test ,clause ,request))))
      (test-against (clause controller) request))))

;;; Processing

(defgeneric action-args (controller request)
  (:documentation "Make action arguments plist out of the request."))

(defmethod succeed ((site site) (controller controller) request)
  (let ((action (action controller)))
    (if action
        (apply action (action-args controller request))
        (fail site request))))

(defmethod action-args ((controller controller) request)
  (let ((decoder (decoder controller))
        (default-args (list :*request* request)))
    (if decoder
        (append default-args (funcall decoder request))
        default-args)))

(defmethod fail ((site site) request)
  (declare (special *response*))
  (setf *response* (no-response site request)))

;;; Link

(defgeneric link (site controller-name &optional args)
  (:documentation "Make a link."))

(defmethod link ((site site) controller-name &optional (args nil))
  (let ((controller (controller site controller-name)))
    (if controller
        (careful-apply (encoder controller) args)
        "/404-broken-link/")))

;; Set controller sugar

(defmacro set-controller (site name &key args link params clause action)
  (with-gensyms (site* name*)
    `(let ((,site* ,site)
           (,name* ,name))
       (mount (make-instance 'controller
                             :name ,name*
                             :encoder (lambda (&key ,@args)
                                        (declare (ignorable ,@args))
                                        ,link)
                             :decoder (lambda (*request*)
                                        (declare (special *request*))
                                        ,params)
                             :clause ',clause
                             :action (lambda (&key *request* ,@args)
                                       (declare (special *request*))
                                       (declare (ignorable ,@args))
                                       ,action)
                             )
              ,site*))))
