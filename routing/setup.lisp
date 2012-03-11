;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defmacro set-special-link (route-key &body body)
  `(defmethod make-link :around ((router (eql *server*))
                                 (route (eql ,route-key))
                                 &rest parameters)
     (macrolet ((another-link (route-key)
                  `(apply #'make-link router ,route-key parameters)))
       (symbol-macrolet ((original-link (call-next-method)))
         ,@body))))

(defun unset-route (name)
  (setf (routes *server*)
        (delete name (routes *server*) :key #'route-name :test #'equal)))

(defmacro set-route (name &key (follow t) args link clause-with-scope scope clause pookies action)
  (once-only (name)
    `(awith (make-instance 'route
                           :name ,name
                           :follow ,follow
                           :args ',args
                           :link ,(aif link
                                       (if args
                                           `(lambda (&key ,@args &allow-other-keys)
                                              (declare (ignorable ,@args))
                                              ,it)
                                           `(lambda (&key)
                                              ,it))
                                       `(lambda (&key)
                                          (broken-link)))
                           :clause (lambda ()
                                     ,(if (and (null clause) (null link))
                                          nil
                                          (let ((clause (or clause
                                                            (awith link
                                                              (when (stringp it)
                                                                `(string= ,it
                                                                          (script-name*)))))))
                                            (if clause-with-scope
                                                `(with-scope ,scope ,clause)
                                                clause))))
                           :action (lambda ()
                                     (with-scope ,scope
                                       (with-pookies ,pookies
                                         ,(or action '(call-next-route))))))
       (unset-route ,name)
       (push it (routes *server*)))))
