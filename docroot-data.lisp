;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defun load-file-datum (name)
  (let ((*package* (server-package *server*))
        (name (string-downcase (symbol-name name))))
    (load-from-file (docroot/ (format nil (server-data-pathname-format *server*) name)))))

(defmacro define-file-datum (name)
  `(let (,name)
     (defun ,name ()
       (multiple-value-bind (data cached)
           (load-file-datum ',name)
         (when (or (null ,name)
                   (null cached))
           (setf ,name (eval `(list ,@data)))))
       ,name)
     (define-symbol-macro ,name (,name))))
