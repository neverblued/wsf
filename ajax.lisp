(in-package #:wsf)

(defgeneric ajax-actions (site))
(defgeneric ajax-action (site action-name))
(defgeneric (setf ajax-action) (new-action site action-name))
(defgeneric ajax-response (site action-name action-args))

(defmethod ajax-action (site action-name)
  (values (gethash action-name (ajax-actions site) nil)))

(defmethod (setf ajax-action) (new-action site action-name)
  (setf (gethash action-name (ajax-actions site))
        new-action))

(defun ajax-win (&optional data)
  (append (list :status "win")
          (awhen data (list :data it))))

(defun ajax-fail (condition)
  (list :status "fail" :data (format nil "~a" condition)))

(defvar *warning-log* nil)

(defmethod ajax-response :around (site action-name action-args)
  (jsun::encode (catch 'ajax-response
                  (handler-case (call-next-method)
                    (warning (condition)
                      (push (cons (get-universal-time) condition) *warning-log*)
                      (muffle-warning condition))
                    (error (condition)
                      (throw 'ajax-response (ajax-fail condition)))))))

(define-condition undefined-ajax-action (error)
  ((action-name :initarg :action-name :reader condition-action-name))
  (:report (lambda (condition stream)
             (format stream "Неизвестное действие ~a." (condition-action-name condition)))))

(defmethod ajax-response (site action-name action-args)
  (let ((action (ajax-action site action-name)))
    (if (functionp action)
        (ajax-win (apply action action-args))
        (error 'undefined-ajax-action :action-name action-name))))

(defmacro set-ajax-route (site &optional (uri "/ajax/"))
  `(set-route ,site :ajax
              :args (action-name action-args)
              :link (join ,uri action-name)
              :clause (begins-with? (request-uri *request*) ,uri)
              :params (list :action-name (name-keyword (trim-left ,uri (request-uri *request*)))
                            :action-args (loop for prm in (post-parameters *request*)
                                            collect (name-keyword (car prm))
                                            collect (cdr prm)))
              :action (setf *response* (make-instance 'text-response :content (ajax-response ,site action-name action-args)))))

(defmacro set-ajax (site action-name action-args &body action-body)
  `(setf (ajax-action ,site ,action-name)
         (lambda (&key ,@action-args)
           (declare (ignorable ,@action-args))
           ,@action-body)))
