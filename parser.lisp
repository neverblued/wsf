(in-package :wsf)

;; model

(defclass text-parser ()
  ((id :initarg :id :accessor parser-id)
   (regex :initarg :regex :accessor parser-regex)
   (patch :initarg :patch :accessor parser-patch)))

(defun parse (site text)
  (let ((*package* (site-package site)))
    (iter (for parser in (site-parsers site))
          (let ((regex (parser-regex parser)))
            (setf text
                  (ppcre:regex-replace-all regex text
                                           (awith (parser-patch parser)
                                             (typecase it
                                               (string it)
                                               (function
                                                (lambda (source &rest patch-args)
                                                 (awith (apply it patch-args)
                                                   (or it source))))))
                                           :simple-calls t)))))
  text)

;;;;

(defun recognize-links (text)
  text)

(defun correct-typography (text)
  text)

(defun parse-content (text &key (recognize-links t) (correct-typography t))
  (if correct-typography
      (correct-typography text)
      (if recognize-links
          (recognize-links text)
          text)))
