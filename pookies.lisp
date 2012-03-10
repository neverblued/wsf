;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defvar *pookies* nil "Список текущих пуков")

(defclass pookie ()
  ((key :initarg :key :accessor pookie-key)
   (var :initarg :var :accessor pookie-var)
   (source :initarg :source :accessor pookie-source)
   (default :initarg :default :accessor pookie-default :initform nil)
   (encoder :initarg :encoder :accessor pookie-encoder :initform nil)))

(defun pookie-value (pookie)
  (or (funcall (pookie-source pookie))
      (funcall (pookie-default pookie))))

(defmacro with-pookie ((key var source &optional default encoder) &body body)
  (with-gensyms (^key^ ^pookie^)
    `(let* ((,^key^ (string-downcase (symbol-name ',key)))
            (,^pookie^ (make-instance 'pookie
                                      :key (symbol-keyword ',key)
                                      :var ',var
                                      :source (lambda ()
                                                (flet ((pookie-origin ()
                                                         (or (get-parameter ,^key^)
                                                             (post-parameter ,^key^))))
                                                  ,source))
                                      :default (lambda ()
                                                 ,default)
                                      :encoder (lambda (it)
                                                 (declare (ignorable it))
                                                 ,encoder)))
            (*pookies* (adjoin ,^pookie^ *pookies*))
            (,var (pookie-value ,^pookie^)))
       (declare (special ,var))
       ,@body)))

(defmacro with-pookies (list-of-pookies &body body)
  (let ((body `(progn ,@body)))
    (labels ((wrap-into-scope (list-of-pookies)
               (aif (first list-of-pookies)
                    `(with-pookie ,it
                       ,(aif (rest list-of-pookies)
                             (wrap-into-scope it)
                             body))
                    body)))
      (wrap-into-scope list-of-pookies))))

(defun pookie-current-value (pookie)
  (symbol-value (pookie-var pookie)))

(defun pookie-default-value (pookie)
  (funcall (pookie-default pookie)))

(defun pookie-encoded-value (pookie)
  (funcall (pookie-encoder pookie)
           (pookie-current-value pookie)))

(defun pookie-still-same? (pookie)
  (equal (pookie-current-value pookie)
         (pookie-default-value pookie)))

(defun pookies ()
  (iter (for pookie in *pookies*)
        (unless (pookie-still-same? pookie)
            (collect (pookie-key pookie))
            (collect (pookie-encoded-value pookie)))))
