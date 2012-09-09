;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defun ajax-win (&optional data)
  (append (list :status "win")
          (awhen data (list :data it))))

(defun ajax-fail (condition)
  (list :status "fail"
        :data (format nil "~a" condition)))

(defvar ajax? nil)

(defvar ajax-action nil)

(defvar ajax-parameters nil)

(defun ajax-parameter (key)
  (getf ajax-parameters key))

(defun ajax-symbol (key)
  (awhen (ajax-parameter key)
    (symb (string-upcase it))))

(defun ajax-string (key)
  (aif (ajax-parameter key)
       it
       ;(format nil "~a" it)
       ;(flexi-streams:octets-to-string it :external-format :utf-8)
       ;(babel:octets-to-string it :encoding :utf-8)
       ""))

(defun ajax-keyword (key)
  (when (ajax-parameter key)
    (awith (ajax-string key)
      (name-keyword it))))

(defun ajax-value (key)
  (safely-read-from-string (ajax-string key)))

(defun ajax-time
    (key)
  (timestamp-js-to-universal (ajax-value key)))

(let ((prefix "/ajax/")
      (response
       `(make-instance 'text-response
                       :content (jsun::encode
                                 (catch 'ajax-response
                                   (handler-case (ajax-win (call-next-route))
                                     (warning (condition)
                                       (muffle-warning condition))
                                     (error (condition)
                                       (ajax-fail condition))))))))

  (defmacro set-route-ajax (&key (uri prefix) follow)
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
