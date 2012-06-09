;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

;; charset

(defparameter *charsets* `(
                           (:utf-8 (,hunchentoot::+utf-8+ "utf-8"))
                           ))

(flet ((? (charset)
         (find-assoc charset *charsets* :test #'eql)))

  (defun charset-instance (charset)
    (first (? charset)))

  (defun charset-string (charset)
    (second (? charset))))

;;; response

(defgeneric content (response))
(defgeneric content-type (response))
(defgeneric charset (response))
(defgeneric status (response))

(defun format-content-type (response &optional stream)
  (format stream "~a; charset=~a" (content-type response) (charset-string (charset response))))

(defun set-reply (response)
  (flet ((hunchentoot-version ()
           (slot-value (asdf:find-system "hunchentoot") 'asdf::version)))
    (setf (return-code*) (status response)
          (reply-external-format*) (charset-instance (charset response))
          (content-type*) (format-content-type response)
          (header-out :server) (format nil "WSF over Hunchentoot ~a" (hunchentoot-version)))))

(defgeneric send (response))

(defmethod send (response)
  (let ((content (content response)))
    (if (and (within-request-p) (boundp '*reply*))
        (set-reply response)
        (format t "~&Return code: ~a~%" (status response)))
    content))

(defmethod content (response)
  (format nil "Hello, ~a!" (gensym "WORLD")))

(defclass response ()
  ((status :initarg :status :accessor status :initform +http-ok+)
   (charset :initarg :charset :accessor charset :initform :utf-8)))

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
   (script :initarg :script :accessor script :initform nil)
   (links :initarg :links :accessor links :initform nil)
   (appendix :initarg :appendix :accessor appendix :initform nil)))

(defgeneric format-html-meta (html-response))
(defgeneric format-html-style (html-response))
(defgeneric format-html-script (html-response))

(defmethod content :around ((response html-response))
    (format nil "<!DOCTYPE html><html><head><title>~a</title>~{~a~}</head><body>~a</body></html>"
            (title response)
            (append (iter (for x in (adjoin '("/favicon.ico"
                                              :rel "shortcut icon"
                                              :type "image/x-icon")
                                            (links response)))
                          (collect (apply #'html-link x)))
                    (list (html-meta-content-type response)
                          (format-html-meta response)
                          (format-html-style response)
                          (format-html-script response)
                          (or (appendix response) "")))
            (call-next-method)))

;; link

(defun html-link (href &key rel type)
  (format nil "<link rel='~a'~a href='~a' />" rel (if type (format nil " type='~a'" type) "") href))

(defparameter uri-base-css "/css/")

(defun html-include-style (path-base)
  (html-link (join uri-base-css path-base ".css") :rel "stylesheet" :type "text/css"))

(defmethod format-html-style ((response html-response))
  (let ((style-names (style response)))
    (if style-names
        (apply #'join (mapcar #'html-include-style style-names))
        "")))

;; script

(defun html-script (href &key type)
  (format nil "<script type='~a' src='~a'></script>" type href))

(defparameter uri-base-js "/js/")

(defun html-include-javascript (path &optional (type "text/javascript"))
  (html-script (if (ppcre:scan "^http://" path) path (join uri-base-js path ".js")) :type type))

(defmethod format-html-script ((response html-response))
  (aif (script response)
       (apply #'join (mapcar #'html-include-javascript it))
       ""))

;; meta

(defun html-meta (name content)
  (format nil "<meta name=~a content=~a />" (jsun::encode name) (jsun::encode content)))

(defun html-meta-content-type (response)
  (format nil "<meta http-equiv=\"content-type\" content=~a />" (jsun::encode (format-content-type response))))

(defmethod format-html-meta ((response html-response))
  (apply #'join
         (iter (for x in (meta-content response))
               (collect (apply #'html-meta x)))))
