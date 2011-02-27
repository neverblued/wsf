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
  (if data
      (list :status "win" :data data)
      (list :status "win")))

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

(defmethod ajax-response (site action-name action-args)
  (let ((action (ajax-action site action-name)))
    (if (functionp action)
        (ajax-win (apply action action-args))
        (ajax-fail (list "unknown-action-name" action-name)))))

(defmacro set-ajax-route (site &optional (uri "/ajax/"))
  `(set-route ,site :ajax
              :args (action-name action-args)
              :link (join ,uri action-name)
              :clause (with-request request-uri begins-with? ,uri)
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
