;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defun file-datum-pathname (name)
  (docroot/ (format nil (server-data-pathname-format *server*)
                    (string-downcase (symbol-name name)))))

(defun load-file-datum (name)
  (let ((*package* (server-package *server*)))
    (handler-case
        (load-from-file (file-datum-pathname name) :cache nil)
      (error (condition)
        (prognil (print condition))))))

(defun save-file-datum (name datum)
  (save-into-file datum (file-datum-pathname name)))

(defun eval-list (data)
  (eval `(list ,@data)))

(defmacro define-file-datum (name &key check)
  (let ((source (symb name "-SOURCE"))
        (cache (symb name "-CACHE"))
        (validator (symb name "-VALIDATOR")))
    `(progn

       (defvar ,cache)

       (defgeneric ,source ())
       (defgeneric (setf ,source) (datum))

       (defmethod ,source ()
         (if (boundp ',cache)
             ,cache
             (eval-list (load-file-datum ',name))))

       (defmethod (setf ,source) (datum)
         (save-file-datum ',name datum)
         (setf ,cache datum))

       (defun ,validator ()
         (unless (awith ,cache ,check)
           (error ',(symb "INVALID-" name))))

       (defun ,(symb "CLEAR-" name) ()
         (setf ,cache nil))

       (defmacro ,(symb "WITH-" name) (&body body)
         `(let (result (,',cache (,',source)))
            (awith ,',cache
              (setf result (progn ,@body))
              (setf ,',cache it)
              (,',validator)
              result))))))
