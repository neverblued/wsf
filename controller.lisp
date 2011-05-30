(in-package #:wsf)

;;; route

(defgeneric route-controller (route))
(defgeneric route-name (route))
(defgeneric route-clause (route))
(defgeneric route-action (route))
(defgeneric route-encoder (route))
(defgeneric route-decoder (route))

(defgeneric route-args (route request))
(defvar *route-args*)

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
   (link :initarg :link :accessor route-link)
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
       (careful-apply (route-link it) args)
       broken-link))

;; setup

(defmacro set-route (controller name &key args track link clause params action)
  `(progn (setf (routes ,controller)
                (delete ,name (routes ,controller) :key #'route-name :test #'equal))
          (push (make-instance 'route
                               :name ,name
                               :link (lambda (&key ,@args)
                                       (declare (ignorable ,@args))
                                       (let (track)
                                         (iter (for (arg . params) in ',track)
                                               (when (listp params)
                                                 (awhen (getf params :save)
                                                   (set arg (eval it))))
                                               (let ((arg-name (string-downcase (symbol-name arg))))
                                                 (awhen (or (symbol-value arg) (get-parameter arg-name))
                                                   (setf (getf track arg-name) it))))
                                         (awhen (bj:split-once "#" ,link)
                                           (let* ((get-parameters (format nil "~{~{~a~^=~}~^&~}" (group track 2)))
                                                  (script-name (if track
                                                                   (join (first it) "?" get-parameters)
                                                                   (first it)))
                                                  (fragment (second it)))
                                             (if fragment
                                                 (join script-name "#" fragment)
                                                 script-name)))))
                               :clause (lambda (*request*)
                                         ,(aif clause it `(string= (script-name*) ,link)))
                               :decoder (lambda (*request*)
                                          ,(awhen params it))
                               :action (lambda (&key *request* ,@args)
                                         (declare (ignorable ,@args))
                                         (let (*route-args*)
                                           ,@(iter (for arg in args)
                                                   (collect `(setf (getf *route-args* ,(name-keyword (symbol-name arg)))
                                                                   ,arg)))
                                           ,action)))
                (routes ,controller))))

(defun unset-route (controller name)
  (setf (routes controller)
        (delete name (routes controller) :key #'route-name :test #'equal)))
