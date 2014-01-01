;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defun default-server-message ()
  (format nil "Hello, ~a!" (gensym "WORLD")))
