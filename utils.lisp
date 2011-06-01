(in-package #:wsf)

(defun assert-type (object type)
  (unless (typep object type)
    (error 'type-error :datum object :expected-type type)))
