(in-package #:wsf)

(defun assert-type (object type)
  (unless (typep object type)
    (error 'type-error :datum object :expected-type type)))

;; plist

(defun unique-plist (plist)
  (let (result)
    (iter (for (key value) in (group plist 2))
          (unless (getf result key)
            (setf (getf result key) value)))
    result))

(defun extend-plist (origin patch)
  (let (result)
    (iter (for (key value) in (group (append origin patch) 2))
          (setf (getf result key) value))
    result))
