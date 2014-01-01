;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defun encode-get-parameters (parameters)
  (when parameters
    (apply #'join-by "&"
           (iter (for (key value) in (group parameters 2))
                 (collect (format nil "~(~a~)=~a" key value))))))

(defun decode-get-parameters (string)
  (plist-unique (iter (for pair in (ppcre:split "\\&" string))
                      (destructuring-bind (name &optional (value ""))
                          (ppcre:split "\\=" pair)
                        (collect (name-keyword name))
                        (collect value)))))

(defun insert-get-parameters (link parameters)
  (destructuring-bind (script &optional fragment)
      (bj:split-once "#" link)
    (let ((uri (if parameters
                   (destructuring-bind (script-name &optional script-args)
                       (bj:split-once "\\?" script)
                     (let* ((script-params (decode-get-parameters script-args))
                            (all-params (plist-unique (append parameters script-params)))
                            (string-params (encode-get-parameters all-params)))
                       (join script-name (aif string-params (join "?" it) ""))))
                   script)))
      (if fragment
          (join uri "#" fragment)
          uri))))
