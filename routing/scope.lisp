;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defvar *scope* nil "Текущее окружение")

(defmacro with-scope (scope &body body)
  `(let* ((*scope* (append ',scope *scope*))
          ,@scope)
     (declare (ignorable ,@(mapcar #'first scope)))
     ,@body))

(defun route-args-scope (route)
  (awhen (route-args route)
    (iter (for key in it)
          (for pair = (find key *scope* :key #'first))
          (awhen pair
            (collect (symbol-keyword key))
            (collect (second it))))))
