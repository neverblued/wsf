(in-package #:wsf)

;; route

(defclass route ()
  ((name :initarg :name :accessor route-name)
   (follow :initarg :follow :accessor route-follow)
   (clause :initarg :clause :accessor route-clause)
   (action :initarg :action :accessor route-action)
   (link :initarg :link :accessor route-link)
   (decoder :initarg :decoder :accessor route-decoder)))

(defvar *parameters*)

(defun *parameters* (route)
  (awhen (route-decoder route)
    (funcall it)))

(defvar *routes* nil "Currently active routes")

(defvar *route* nil "Currently active route")

(defgeneric *route* (key)
  (:documentation "Find active route"))

(defmethod *route* ((key route))
  (find key *routes*))

(defmethod *route* ((key symbol))
  (find key *routes* :key #'route-name :test #'equal))

(defmethod *route* ((key string))
  (*route* (name-keyword key)))

(defmethod *route* ((key list))
  (remove-if-not #'*route* key))

(defmethod *route* ((key (eql t)))
  t)

;; router

(defvar *router* nil "Currently active router")

(defclass router ()
  ((routes :accessor routes :initform nil)))

(defmacro with-router (router &body body)
  "Evaluate in *ROUTER* binding"
  `(let ((*router* ,router))
     (assert-type *router* 'router)
     ,@body))

(defun route (key)
  (let ((*routes* (routes *router*)))
    (*route* key)))

(defun route! ()
  (iter (for route in (set-difference (routes *router*) *routes*))
        (when (and (*route* (route-follow route))
                   (funcall (route-clause route)))
          (let ((*routes* (adjoin route *routes*)))
            (leave (aif (route-action route)
                        (let ((*route* route))
                          (apply it (*parameters* route)))
                        (route!)))))))

;;; link

(defgeneric link (route-key &rest args)
  (:documentation "Make a link."))

(defparameter broken-link "/404-broken-link/")

(defmethod link (route-key &rest parameters)
  (aif (route route-key)
       (careful-apply (route-link it) parameters)
       broken-link))

;; setup

(defun insert-get-parameters (link parameters)
  (awhen (bj:split-once "#" link)
    (let ((script (first it))
          (fragment (second it)))
      (let ((uri (if (< 0 (hash-table-count parameters))
                     (let ((get-parameters (format nil "~{~{~a~^=~}~^&~}"
                                                   (maphash-collect (k v) parameters
                                                     (list k v)))))
                       (join script "?" get-parameters))
                     script)))
        (if fragment
            (join uri "#" fragment)
            uri)))))

(defmacro set-route (name &key (follow t) args track link clause params action)
  `(progn (setf (routes *router*)
                (delete ,name (routes *router*) :key #'route-name :test #'equal))
          (push (make-instance 'route
                               :name ,name
                               :follow ,follow
                               :link (lambda (&key ,@args)
                                       (declare (ignorable ,@args))
                                       (let ((track (make-hash-table :test #'equal)))
                                         (iter (for (arg . params) in ',track)
                                               (when (listp params)
                                                 (awhen (getf params :save)
                                                   (set arg (eval it))))
                                               (let ((arg-name (string-downcase (symbol-name arg))))
                                                 (awhen (or (symbol-value arg) (get-parameter arg-name))
                                                   (setf (gethash arg-name track) it))))
                                         (insert-get-parameters ,link track)))
                               :clause (lambda ()
                                         ,(aif clause it `(string= (script-name*) ,link)))
                               :decoder (lambda ()
                                          ,(awhen params it))
                               :action (lambda (&key ,@args)
                                         (declare (ignorable ,@args))
                                         ,action))
                (routes *router*))))

(defun unset-route (name)
  (setf (routes *router*)
        (delete name (routes *router*) :key #'route-name :test #'equal)))
