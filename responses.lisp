(in-package #:wsf)

(pushnew +http-not-found+ *approved-return-codes*)
(pushnew +http-internal-server-error+ *approved-return-codes*)

;; charset

(defparameter *charsets* `(
                           (:utf-8 (,hunchentoot::+utf-8+ "utf-8"))
                           ))

(defun charset-instance (charset)
  (first (find-assoc charset *charsets* :test #'eql)))

(defun charset-string (charset)
  (second (find-assoc charset *charsets* :test #'eql)))

;;; response

(defgeneric content (response))
(defgeneric content-type (response))
(defgeneric charset (response))
(defgeneric status (response))

(defun send (response)
  (let ((content (content response)))
    (when (boundp '*reply*)
      (setf (return-code* *reply*)                (status response)
            (content-type* *reply*)               (format-content-type response)
            *hunchentoot-default-external-format* (charset-instance (charset response))))
    content))

(defmethod content (response)
  (format nil "Hello, ~a!" (gensym "WORLD")))

(defun format-content-type (response)
  (format nil "~a; charset=~a"
          (content-type response)
          (charset-string (charset response))))

(defclass response ()
  ((status :initarg :status :accessor status :initform +http-ok+)
   (charset :initarg :charset :accessor charset :initform :utf-8)))

;;; file response

(defclass file-response (response)
  ((file-path :initarg :file-path :accessor file-path)))

(defmethod content-type ((response file-response))
  (mime-type (file-path response)))

(defmethod content ((response file-response))
  (pathname-content (file-path response) :binary t))
  ;(multiple-value-bind (content cached?)
  ;    (pathname-content (file-path response) :binary t)
  ;  (when cached?
  ;    (setf (status response) +http-not-modified+))
  ;  content))

;;; text response

(defclass text-response (response)
  ((content-type :initarg :content-type :accessor content-type :initform "text/plain")
   (content :initarg :content :accessor content :initform "Hello, world!")))

;;; HTML response

(defclass html-response (text-response)
  ((title :initarg :title :accessor title :initform "Hello, world!")
   (content-type :initform "text/html")
   (content :initform "<h1>Hello, world!</h1><p>I'm a <i>markup</i>.</p>")
   (meta-content :initarg :meta :accessor meta-content :initform nil)
   (style :initarg :style :accessor style :initform nil)
   (script :initarg :script :accessor script :initform nil)))

(defgeneric format-html-meta (html-response))
(defgeneric format-html-style (html-response))
(defgeneric format-html-script (html-response))

(defmethod content ((response html-response))
  (format nil "<!DOCTYPE html><html><head><title>~a</title>~{~a~}</head><body>~a</body></html>"
          (title response)
          (list (format-html-meta response)
                (format-html-style response)
                (format-html-script response)
                (html-link "/favicon.ico" :rel "shortcut icon" :type "image/x-icon"))
          (slot-value response 'content)))

;; link

(defun html-link (href &key rel type)
  (format nil "<link rel='~a' type='~a' href='~a' />" rel type href))

(defun html-include-style (path-base)
  (html-link (join "/css/" path-base ".css") :rel "stylesheet" :type "text/css"))

(defmethod format-html-style ((response html-response))
  (let ((style-names (style response)))
    (if style-names
        (apply #'join (mapcar #'html-include-style style-names))
        "")))

;; script

(defun html-script (href &key type)
  (format nil "<script type='~a' src='~a'></script>" type href))

(defun html-include-javascript (path-base &optional (type "text/javascript"))
  (html-script (join "/js/" path-base ".js") :type type))

(defmethod format-html-script ((response html-response))
  (aif (script response)
       (apply #'join (mapcar #'html-include-javascript it))
       ""))

;; meta

(defun html-meta (name content)
  (format nil "<meta name=~a content=~a />" (jsun::encode name) (jsun::encode content)))

(defmethod format-html-meta ((response html-response))
  (aif (meta-content response)
       (apply #'join (mapcar (lambda (meta)
                               (apply #'html-meta meta))
                             it))
       ""))
