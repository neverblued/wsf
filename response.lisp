(in-package #:wsf)

;;; ~~~~~~~~
;;; Response
;;; ~~~~~~~~

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

(defgeneric no-response (site))

(defun sendable? (thing)
  (typep thing 'response))

(defparameter *response* nil)

(defmethod respond :around ((site site) (request hunchentoot::request))
  (let (*response*)
    (declare (special *response*))
    (call-next-method)
    (send (if (sendable? *response*)
              *response*
              (no-response site)))))


(defgeneric format-content-type (response))

(defmethod send :before ((response response) &key)
  (if (boundp 'hunchentoot::*reply*)
      (setf (hunchentoot::content-type* hunchentoot::*reply*)
            (format-content-type response)
            hunchentoot::*hunchentoot-default-external-format*
            (charset-instance (charset response)))
      (print (content response))))

(defmethod format-content-type ((response response))
  (format nil "~a; charset=~a" (content-type response) (charset-string (charset response))))

(defmethod content ((response response))
  "Hello, world!")

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

(defmethod send ((response file-response) &key (binary t))
  (string<-pathname (file-path response) :binary binary))

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
   (title :initform "&last; &last; &last;"
          :initarg :title
          :accessor title
          )
   (content-type :initform "text/html"
                 )
   (content :initform "Hello, world! I am a WSF::HTML-RESPONSE template content."
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

(defun build-html-link (href &key rel type)
  (format nil "<link rel='~a' type='~a' href='~a' />" rel type href))

(defun build-html-link-style (path-base)
  (build-html-link (join "/css/" path-base ".css") :rel "stylesheet" :type "text/css"))

(defmethod build-html-style ((response html-response))
  (if (style response)
      (apply #'join
             (mapcar #'build-html-link-style (style response)))
      ))

(defun build-html-script (href &key type)
  (format nil "<script type='~a' src='~a'></script>" type href))

(defun build-html-scripts-include (path-base &optional (type "text/javascript"))
  (build-html-script (join "/js/" path-base ".js") :type type))

(defmethod build-html-scripts ((response html-response))
  (if (scripts response)
      (apply #'join
             (mapcar #'build-html-scripts-include (scripts response)))
      ))

(defmethod send ((response html-response) &key)
  (apply #'join-rec
         (patch '(
                  "<!DOCTYPE html>"
                  "<html>"
                  (
                   "<head>"
                   (
                    "<title>" title "</title>"
                    favicon
                    style
                    scripts
                    )
                   "</head>"
                   "<body>" content "</body>"
                   )
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
                  (content (content ,response))
                  ))))

;;; ~~~~~~~~~~~~~~~~~
;;; Template Response
;;; ~~~~~~~~~~~~~~~~~

(defmethod no-response ((site site))
  (declare (ignore site))
  (make-instance 'text-response
                 :content (join "Unfortunately, the website has no"
                                " appropriate content for your request"
                                " (no-response)."
                                )
                 ))
