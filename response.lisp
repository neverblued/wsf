(in-package #:dc.wsf)

;;; ~~~~~~~~~~~~~
;;; File Response
;;; ~~~~~~~~~~~~~

(defclass file-response (site-response)
  (
   (file-path :initarg :file-path
              :accessor file-path
              )
   ))

(defmethod content-type ((response file-response))
  (pathname->mime (file-path response)))

(defmethod send ((response file-response) &key (binary t))
  (pathname->string (file-path response) :binary binary))

;;; ~~~~~~~~~~~~~
;;; Text Response
;;; ~~~~~~~~~~~~~

(defclass text-response (site-response)
  (
   (content-type :initform "text/plain"
                 )
   (content :initform "Hello, world! I am a WSF::TEXT-RESPONSE template content."
            )
   ))

;;; ~~~~~~~~~~~~~
;;; HTML Response
;;; ~~~~~~~~~~~~~

(defclass title-haver ()
  (
   (title :initform "&last; &last; &last;"
          :initarg :title
          :accessor title
          )
   ))

(defclass html-response (title-haver text-response)
  (
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
  (build-html-link (join "/decore/css/" path-base ".css") :rel "stylesheet" :type "text/css"))

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
                  (favicon ,(build-html-link "/decore/favicon.ico" :rel "shortcut icon" :type "image/x-icon"))
                  (content (content ,response))
                  ))))

;;; ~~~~~~~~~~~~~~~~~~
;;; Template Responses
;;; ~~~~~~~~~~~~~~~~~~

(defmethod response[test] ((site site))
  (make-instance 'text-response
                 ))

(defmethod response[not-implemented] ((site site))
  (make-instance 'text-response
                 ;:headers '(
                 :content (join "Unfortunately, the website is not implemented yet "
                                "(reply #501).")
                 ))

(defmethod response[not-found] ((site site))
  (make-instance 'text-response
                 :content (join "Unfortunately, the website has no "
                                "appropriate content for your request "
                                "(reply #404)."
                                )
                 ))

(defun is-response? (x)
  (and x (typep x 'site-response)))