;; (c) www.neverblued.info
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
        (delete name (routes *server*) :test #'equal :key #'route-name)))

(defun set-route (this router name &optional (follow t))
  (with-server router
    (unset-route name)
    (setf (route-name this) name
          (route-follow this) follow)
    (push this (routes *server*))))

(defmacro script-name= (link)
  `(ignore-errors
     (string= (script-name*) ,link)))

(defmacro make-clause (args link clause-with-scope scope clause)
  `(lambda-keys (,@args)
     ,(if (and (null args) (null clause))
          `(script-name= ,link)
          (if clause-with-scope
              `(with-scope ,scope ,clause)
              clause))))

(defmacro make-action (scope pookies body)
  `(lambda ()
     (with-scope ,scope
       (with-pookies ,pookies
         ,(or body '(call-next-route))))))

(defmacro make-route (args link clause-with-scope scope clause pookies action)
  (let ((link (if (null link)
                  `(lambda (&key) (broken-link))
                  `(lambda-keys (,@args) ,link)))
        (clause (if (and (null clause) (null args) (null link))
                    (error "Null CLAUSE, ARGS and LINK.")
                    `(make-clause ,args ,link
                                  ,clause-with-scope ,scope ,clause)))
        (action `(make-action ,scope ,pookies ,action)))
    (awith `(:args ',args :link ,link :clause ,clause :action ,action)
      `(make-instance 'route ,@it))))

(defmacro defroute
    ((router name &key (follow t))
     &key args link clause-with-scope scope clause pookies action)
  (once-only (router name follow)
    (awith `(make-route ,args ,link ,clause-with-scope
                        ,scope ,clause ,pookies ,action)
      `(set-route ,it ,router ,name ,follow))))
