(in-package #:wsf)

(defun ajax-win (&optional data)
  (append (list :status "win")
          (awhen data (list :data it))))

(defun ajax-fail (condition)
  (list :status "fail"
        :data (format nil "~a" condition)))

(defvar *warning-log* nil)

(defvar ajax? nil)

(defvar ajax-data)

(defun ajax-datum (key)
  (getf ajax-data key))

(let ((ajax-action
       `(make-instance 'text-response
                       :content (jsun::encode
                                 (catch 'ajax-response
                                   (handler-case (ajax-win (call-next-route))
                                     (warning (condition)
                                       (push (cons (get-universal-time) condition)
                                             *warning-log*)
                                       (muffle-warning condition))
                                     (error (condition)
                                       (ajax-fail condition))))))))

  (defmacro set-route-ajax (&key (uri "/ajax/") follow)
    `(set-route :ajax
                :follow ,follow
                :args (action-name)
                :link (join ,uri action-name)
                :clause (begins-with? (script-name*) ,uri)
                :scope ((ajax-action-name (trim-left ,uri (script-name*))))
                :action (let ((ajax? t)
                              (ajax-data (list :action-name (name-keyword ajax-action-name)
                                               :parameters (iter (for prm in (post-parameters*))
                                                                 (collect (name-keyword (car prm)))
                                                                 (collect (cdr prm))))))
                          ,ajax-action))))
