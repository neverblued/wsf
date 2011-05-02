(in-package #:wsf)

(defun ajax-action (site action-name)
  (values (gethash action-name (ajax-actions site) nil)))

(defun (setf ajax-action) (new-action site action-name)
  (setf (gethash action-name (ajax-actions site))
        new-action))

(defun pprint-ajax (site &optional (stream t))
  (maphash (lambda (action-name func)
             (format stream "~&~a => ~a~%" action-name func))
           (ajax-actions site)))

(defun ajax-win (&optional data)
  (append (list :status "win")
          (awhen data (list :data it))))

(defun ajax-fail (condition)
  (list :status "fail" :data (format nil "~a" condition)))

(defvar *warning-log* nil)

(defun ajax-response (site action-name action-args)
  (jsun::encode (catch 'ajax-response
                  (handler-case (aif (ajax-action site action-name)
                                     (ajax-win (apply it action-args))
                                     (error 'undefined-ajax-action :action-name action-name))
                    (warning (condition)
                      (push (cons (get-universal-time) condition) *warning-log*)
                      (muffle-warning condition))
                    (error (condition)
                      (throw 'ajax-response (ajax-fail condition)))))))

(defmacro set-ajax-routing (site &optional (uri "/ajax/"))
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
