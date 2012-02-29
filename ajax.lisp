(in-package #:wsf)

(defun ajax-win (&optional data)
  (append (list :status "win")
          (awhen data (list :data it))))

(defun ajax-fail (condition)
  (list :status "fail"
        :data (format nil "~a" condition)))

(defvar *warning-log* nil)

(defvar ajax? nil)

(defvar ajax-action nil)

(defvar ajax-parameters nil)

(defun ajax-parameter (key)
  (getf ajax-parameters key))

(defun ajax-string (key)
  (getf ajax-parameters key ""))

(defun ajax-keyword (key)
  (when (ajax-parameter key)
    (awith (ajax-string key)
      (name-keyword it))))

(defun ajax-value (key)
  (safely-read-from-string (ajax-string key)))

(let ((response
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
    (once-only (uri)
      `(set-route :ajax
                  :follow ,follow
                  :args (action-name)
                  :link (join ,uri action-name)
                  :clause (begins-with? (script-name*) ,uri)
                  :scope ((action-name (trim-left ,uri (script-name*))))
                  :action (let ((ajax? t)
                                (ajax-action (name-keyword action-name))
                                (ajax-parameters (iter (for prm in (post-parameters*))
                                                       (collect (name-keyword (car prm)))
                                                       (collect (cdr prm)))))
                            ,response)))))
