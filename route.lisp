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
(defparameter last-requests nil)

(defmethod respond ((site site) (request hunchentoot::request))
  (setf last-request request)
  (setf last-requests
        (cons last-request
              (if (> 5 (length last-requests))
                  last-requests
                  (subseq last-requests 0 4))))
  (direct site request))

(defgeneric clause (route)
  (:documentation "Clause that should succeed to choose the site-route."))

(defgeneric action (route)
  (:documentation "Controller of the site-route."))

(defclass site-route (route)
  (
   (clause :initarg :clause
           :accessor clause
           :initform nil
           )
   (action :initarg :action
           :accessor action
           :initform nil
           )
   ))

;(defun integrate-test (clause request)
;  `(macrolet ((test (getter tester example)
;                `(clause-match? (make-clause ,getter ,tester ,example) ,',request)))
;     ,clause))

;(defmacro test-against (clause request)
;  `(eval (integrate-test ,clause ,request)))

(defmethod match ((request hunchentoot::request) (route site-route))
  (flet ((integrate-test (clause request)
           `(macrolet ((test (getter tester example)
                         `(clause-match? (make-clause ,getter ,tester ,example) ,',request)))
              ,clause)))
    (macrolet ((test-against (clause request)
                 `(eval (integrate-test ,clause ,request))))
      (test-against (clause route) request))))

(defgeneric act (route request)
  (:documentation "Run the action of the route with the request."))

(defmethod succeed ((site site) (route site-route) request)
  (act route (decode route request)))

(defmethod fail ((site site) request)
  (let ((route-404 (route site :404)))
    (if route-404
        (act route-404 (list :*request* request))
        (setf *response*
              (no-response site)))))

(defmethod act ((route site-route) args)
  (let ((action (action route)))
    (when action
      (apply action args))))

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
;  (declare (ignore site route-name params))
;  "/404-links-not-implemented/")
  (let ((route (route site route-name)))
    (if route
        (encode route params)
        "/404/")))

(defmacro mount-route (site name &key args link params clause action)
  (with-gensyms (site* name*)
    `(let ((,site* ,site)
           (,name* ,name))
       (mount (make-instance 'site-route
                             :name ,name*
                             :encoder (lambda (&key ,@args)
                                        (declare (ignorable ,@args))
                                        ,link)
                             :decoder (lambda (*request*)
                                        (declare (special *request*))
                                        ,params)
                             :clause ',clause
                             :action (lambda (&key ,@args)
                                       (declare (ignorable ,@args))
                                       ,action)
                             )
              ,site*))))
