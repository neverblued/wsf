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

(defgeneric controller-clause (controller)
  (:documentation "Clause that should evaluate to T for matching the controller."))

(defgeneric controller-action (controller)
  (:documentation "Action of the controller."))

(defgeneric controller-encoder (controller)
  (:documentation "Controller encoder."))

(defgeneric controller-decoder (controller)
  (:documentation "Controller decoder."))

(defclass controller (route)
  (
   (controller-clause :initarg :clause
                      :accessor controller-clause
                      )
   (controller-action :initarg :action
                      :accessor controller-action
                      )
   (controller-decoder :initarg :decoder
                       :accessor controller-decoder
                       )
   (controller-encoder :initarg :encoder
                       :accessor controller-encoder
                       )
   ))

;;; Match

(defmethod match ((request hunchentoot::request) (controller controller))
  (let ((clause (controller-clause controller)))
    (funcall clause request)))

;    (macrolet ((run-test (clause request)
;                 (eval `(integrate-test ,clause ,request))))
;      (run-test clause request))))

;;; Processing

(defgeneric action-args (controller request)
  (:documentation "Make action arguments plist out of the request."))

(defmethod respond-success ((site site) (controller controller) request)
  (let ((action (controller-action controller)))
    (if action
        (apply action (action-args controller request))
        (respond-failure site request))))

(defmethod action-args ((controller controller) request)
  (let ((decoder (controller-decoder controller))
        (default-args (list :*request* request)))
    (if decoder
        (append default-args (funcall decoder request))
        default-args)))

(defmethod respond-failure ((site site) request)
  (declare (special *response*))
  (setf *response* (response-404 site request)))

;;; Link

(defgeneric link (site controller-name &optional args)
  (:documentation "Make a link."))

(defmethod link ((site site) controller-name &optional (args nil))
  (let ((controller (site-controller site controller-name)))
    (if controller
        (careful-apply (controller-encoder controller) args)
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
                             :decoder (lambda (request)
                                        (let ((*request* request))
                                          (declare (special *request*))
                                          ,params))
                             :clause (lambda (request)
                                       (let ((*request* request))
                                         (declare (special *request*))
                                         (macrolet ((with-request (getter tester example)
                                                      `(clause-match? (make-clause ,getter ,tester ,example) *request*)))
                                           ,clause)))
                             :action (lambda (&key *request* ,@args)
                                       (declare (special *request*))
                                       (declare (ignorable ,@args))
                                       ,action)
                             )
              ,site*))))
