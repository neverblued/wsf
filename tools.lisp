;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defmacro push-trim (item list &key limit)
  (once-only (limit)
    `(setf ,list
           (cons ,item
                 (awith ,list
                   (if (> ,limit (length it))
                       it
                       (subseq it 0 (1- ,limit))))))))

(defmacro lambda-keys ((&rest args) &body body)
  (if (null args)
      `(lambda (&key)
         ,@body)
       `(lambda (&key ,@args &allow-other-keys)
          (declare (ignorable ,@args))
          ,@body)))
