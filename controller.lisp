(in-package #:wsf)

;;; Clause

(defmacro make-clause (get test example)
  `(list #',get #',test ,example))

(defun clause-get (clause)
  (first clause))

(defun clause-test (clause)
  (second clause))

(defun clause-example (clause)
  (third clause))

(defun clause-match? (clause request)
  (funcall (clause-test clause)
           (funcall (clause-get clause) request)
           (clause-example clause)))

;;; Controller

(defclass controller (selector)
  ())

(defgeneric page-clause (site-page))

(defgeneric page-action (site-page))

(defgeneric page-encoder (site-page))

(defgeneric page-decoder (site-page))

(defgeneric page-args (site-page request)
  (:documentation "Make action arguments plist out of the request."))

(defclass site-page (option)
  (
   (page-clause :initarg :clause
                :accessor page-clause
                )
   (page-action :initarg :action
                :accessor page-action
                )
   (page-encoder :initarg :encoder
                 :accessor page-encoder
                 )
   (page-decoder :initarg :decoder
                 :accessor page-decoder
                 )
   ))

(defmethod page-args ((site-page site-page) request)
  (let ((decoder (page-decoder site-page))
        (default-args (list :*request* request)))
    (if decoder
        (append default-args (funcall decoder request))
        default-args)))

(defmethod match? ((request hunchentoot::request) (site-page site-page))
  (let ((clause (page-clause site-page)))
    (funcall clause request)))

;;; Result

(defmethod selection-success ((controller controller) request (site-page site-page))
  (let ((action (page-action site-page)))
    (if action
        (apply action (page-args site-page request))
        (selection-failure controller request))))

(defmethod selection-failure ((controller controller) request)
  (declare (ignore controller request)))

;;; Link

(defgeneric link (site controller-name &optional args)
  (:documentation "Make a link."))

(defmethod link ((site site) page-name &optional (args nil))
  (let ((site-page (select-by-name (site-controller site) page-name)))
    (if site-page
        (careful-apply (page-encoder site-page) args)
        "/404-broken-link/")))

;; Set controller sugar

(defmacro set-site-page (site name &key args link clause params action)
  `(set-selector-option (site-controller ,site)
                        'site-page ,name
                        :encoder (lambda (&key ,@args)
                                   (declare (ignorable ,@args))
                                   ,link)
                        :decoder (lambda (request)
                                   (let ((*request* request))
                                     (declare (special *request*))
                                     ,params))
                        :clause (lambda (request)
                                  (let ((*request* request))
                                    (declare (special *request*))
                                    (macrolet ((with-request (getter tester example)
                                                 `(clause-match? (make-clause ,getter ,tester ,example) *request*)))
                                      ,clause)))
                        :action (lambda (&key *request* ,@args)
                                  (declare (special *request*))
                                  (declare (ignorable ,@args))
                                  ,action)
                        ))
