(in-package #:wsf)

;; example: (make-clause request-uri string= "/favicon.ico")
(defmacro make-clause (get test example)
  `(list #',get #',test ,example))

(defun clause-get (clause) (first clause))
(defun clause-test (clause) (second clause))
(defun clause-example (clause) (third clause))

(defun clause-match? (clause request)
  (funcall (clause-test clause)
           (funcall (clause-get clause) request)
           (clause-example clause)))

(defparameter last-request nil)

(defmethod respond ((site site) (request hunchentoot::request))
  (setf last-request request)
  (direct site request))

(defgeneric clause (route)
  (:documentation "Clause that should succeed to choose the site-route."))

(defgeneric controller (route)
  (:documentation "Controller of the site-route."))

(defclass site-route (route)
  (
   (clause :initarg :when
           :accessor clause
           :initform nil
           )
   (controller :initarg :do
               :accessor controller
               :initform nil
               )
   ))

(defmethod match ((request hunchentoot::request) (route site-route))
  (let ((*request* request))
    (declare (special *request*))
    (macrolet ((eval-clause-with-test (clause)
                 (with-gensyms (clause*)
                   `(let ((,clause* ,clause))
                      (macrolet ((test (get test example)
                                   `(clause-match? (make-clause #',get #',test ,example) *request*)))
                        ,clause*)))))
      (eval-clause-with-test (clause route)))))

(defgeneric control (route request)
  (:documentation "Run the controller of the route with the request."))

(defmethod succeed ((site site) (route site-route) request)
  (control route request))

(defmethod fail ((site site) request)
  (let ((route-404 (route site :404)))
    (if route-404
        (control route-404 request)
        (setf *response*
              (no-response site)))))

(defmethod control ((route site-route) request)
  (let ((controller (controller route)))
    (when controller
      (funcall controller request))))

;(defmethod decode :before ((route site-route) request)
;  nil)
;  (flet ((trim-location (uri route)
;           (cl-ppcre::regex-replace (join "^" (uri route) "(.*)$")
;                                    uri
;                                    "\\1")))
;    (setf (get request :uri)
;          (trim-location (hunchentoot::url-decode (hunchentoot::request-uri request))
;                         route))))

(defmethod link ((site site) route-name &optional (params nil))
  (declare (ignore site route-name params))
  "/404-links-not-implemented/")

;  (let ((route (route site route-name)))
;    (if route
;        (let ((request (encode route params)))
;          (join (uri route) (getf location :uri "")))
;        "/404/")))

(defmacro mount-route (site name &key when do)
  (with-gensyms (site* name*)
    `(let ((,site* ,site)
           (,name* ,name))
       (mount (make-instance 'site-route
                             :name ,name*
                             :when ',when
                             :do ,do)
              ,site*))))
