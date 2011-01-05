(in-package #:wsf)

;;; ~~~~~~~~
;;; Response
;;; ~~~~~~~~

(defgeneric send (response))

(defgeneric content-type (response))

(defgeneric charset (response))

(defun format-content-type (response)
  (format nil "~a; charset=~a"
          (content-type response)
          (charset-string (charset response))))

(defmethod send :before (response)
  (when (boundp '*reply*)
    (setf (content-type* *reply*)
          (format-content-type response)
          *hunchentoot-default-external-format*
          (charset-instance (charset response)))))

(defgeneric content (response))

(defmethod send (response)
  (content response))

(defmethod content (response)
  "Hello, world!")

(defclass response ()
  (
   (content-type :initform "text/plain"
                 :initarg :content-type
                 :accessor content-type
                 )
   (charset :initform :utf-8
            :initarg :charset
            :accessor charset
            )
   ))

;;; ~~~~~~~~~~~~~
;;; File Response
;;; ~~~~~~~~~~~~~

(defclass file-response (response)
  (
   (file-path :initarg :file-path
              :accessor file-path
              )
   ))

(defmethod content-type ((response file-response))
  (mime<-pathname (file-path response)))

(defmethod content ((response file-response))
  (string<-pathname (file-path response) :binary t))

;;; ~~~~~~~~~~~~~
;;; Text Response
;;; ~~~~~~~~~~~~~

(defclass text-response (response)
  (
   (content-type :initform "text/plain"
                 )
   (content :initform "Hello, world! I am a WSF::TEXT-RESPONSE template content."
            :initarg :content
            :accessor content
            )
   ))

;;; ~~~~~~~~~~~~~
;;; HTML Response
;;; ~~~~~~~~~~~~~

(defclass html-response (text-response)
  (
   (title :initform "Hello, world!"
          :initarg :title
          :accessor title
          )
   (content-type :initform "text/html"
                 )
   (content :initform "Hello, world! I'm (WSF::RESPONSE-CONTENT WSF::HTML-RESPONSE)."
            )
   (style :initform nil
          :initarg :style
          :accessor style
          )
   (scripts :initform nil
            :initarg :scripts
            :accessor scripts
            )
   ))

;; links

(defun build-html-link (href &key rel type)
  (format nil "<link rel='~a' type='~a' href='~a' />" rel type href))

(defun build-html-link-style (path-base)
  (build-html-link (join "/css/" path-base ".css") :rel "stylesheet" :type "text/css"))

(defgeneric build-html-style (html-response))

(defmethod build-html-style ((response html-response))
  (let ((style (style response)))
    (if style
        (apply #'join (mapcar #'build-html-link-style style))
        "")))

;; scripts

(defun build-html-script (href &key type)
  (format nil "<script type='~a' src='~a'></script>" type href))

(defun build-html-scripts-include (path-base &optional (type "text/javascript"))
  (build-html-script (join "/js/" path-base ".js") :type type))

(defgeneric build-html-scripts (html-response))

(defmethod build-html-scripts ((response html-response))
  (let ((scripts (scripts response)))
    (if scripts
        (apply #'join (mapcar #'build-html-scripts-include scripts))
        "")))

;; document

(defmethod send ((response html-response))
  (apply #'join-rec
         (patch '(
                  "<!DOCTYPE html>"
                  "<html>"
                  "<head><title>" title "</title>" favicon style scripts "</head>"
                  "<body>" content "</body>"
                  "</html>"
                  )
                `(
                  (style ,(or (build-html-style response)
                              "<!-- CSS be here must, young Padawan! -->"
                              ))
                  (scripts ,(or (build-html-scripts response)
                                "<!-- Scripts be here could, young Padawan! -->"
                                ))
                  (title (title ,response))
                  (favicon ,(build-html-link "/images/favicon.ico" :rel "shortcut icon" :type "image/x-icon"))
                  (content (content ,response))))))
