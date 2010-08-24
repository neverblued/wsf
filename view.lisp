(defpackage #:dc.wsf.view
  (:use
     #:common-lisp
     #:cl-ppcre
     )
  (:export
     #:basic-template
     #:list-template
     #:menu-template
     #:template.load
     #:view
     )
  )

(in-package #:dc.wsf.view)

;;; ~~~~~~~~~
;;; Templates
;;; ~~~~~~~~~

;(defparameter *patch-section-slot-format* "{{slot:patch-section=~a}}")
(defparameter *slot-regex* "\\{\\{slot:([^\\}]+)\\}\\}")
(defparameter *section-regex*
  (cl-ppcre::create-scanner '(:sequence "{{section:"
                                        (:register (:non-greedy-repetition 0 nil :everything))
                                        "}}"
                                        (:regex "\\s*")
                                        (:register (:non-greedy-repetition 0 nil :everything))
                                        (:regex "\\s*")
                                        "{{/section}}"
                              )
                            :single-line-mode t))
(defparameter *separate-section-name-params-regex* "\\?")
(defparameter *comment-regex* (cl-ppcre::create-scanner '(:sequence "{0y0}"
                                                                    (:non-greedy-repetition 0 nil :everything)
                                                                    "{-y-}"
                                                          )
                                                        :single-line-mode t))

;(defun split-once (regex source)
;  (cl-ppcre::split regex source :limit 2))

;(defun with-package-read-string (*package* source)
;  (eval (read-from-string source)))

;(defun parse-source (source package)
;  (macrolet ((bind-profit+scrap (regex source &body body)
;               (let ((profit+scrap (gensym "profit+scrap")))
;                 `(let* (
;                         (,profit+scrap (split-once ,regex ,source))
;                         (profit (first ,profit+scrap))
;                         (scrap  (second ,profit+scrap))
;                         )
;                    ,@body))))
;    (labels (
;             (remove-comments (source)
;               (cl-ppcre::regex-replace-all *regex-comment* source ""))
;             (parse-close (source)
;               (and source
;                    (bind-profit+scrap *regex-open* source
;                      (if scrap
;                          `(,profit ,@(parse-open scrap))
;                          `(,profit)))))
;             (parse-open (source)
;               (and source
;                    (bind-profit+scrap *regex-close* source
;                      (if scrap
;                          `(,(with-package-read-string package profit) ,@(parse-close scrap))
;                          `(,(with-package-read-string package profit))))))
;             )
;      (parse-close (remove-comments source)))))

;(defun parse-source-with-sections (source sections package)
;  (parse-source (cl-ppcre::regex-replace-all *regex-section* source "(section)")
;                package))

;;; ~~~~~~~~~
;;; Templates
;;; ~~~~~~~~~

(defmacro template.load (template pathname)
  `(make-instance ',template
                  :markup (dc.utils:pathname->string ,pathname)))

(defun patch-slots (markup plist)
  (cl-ppcre::regex-replace-all *slot-regex*
                               markup
                               (lambda (match key &rest registers)
                                 (declare (ignorable match registers))
                                 (getf plist (dc.utils:string->keyword key) ""))
                               :simple-calls t
                               ))

;; Basic

(defclass basic-template ()
  (
   (markup :initform ""
           :initarg :markup
           :accessor markup)
   ))

(defmethod initialize-instance :after ((template basic-template) &key)
  (setf (markup template)
        (cl-ppcre::regex-replace-all *comment-regex* (markup template) "")))

(defgeneric view (template &optional data))

(defmethod view ((template basic-template) &optional (data nil))
  (patch-slots (markup template) data))

;; Sections

(defclass sections-template (basic-template)
  (
   (sections :initform nil
             :accessor sections)
   ))

(defclass section ()
  (
   (alias :initarg :alias
          :reader alias
          )
   (params :initarg :params
           :initform nil
           :accessor params
           )
   (template :initarg :template
             :reader template
             )
   ))

(defun decode-section-params (string-source)
  (let (params)
    (loop for token in (cl-ppcre::split "&" string-source)
       do (destructuring-bind (key value)
              (cl-ppcre::split "=" token :limit 2)
            (setf (getf params (dc.utils:string->keyword key))
                  value)))
    params))

(defmethod initialize-instance :after ((template sections-template) &key)
  (setf (markup template)
        (cl-ppcre::regex-replace-all *section-regex*
                                     (markup template)
                                     (lambda (match alias content &rest registers)
                                       (declare (ignore match registers))
                                       (destructuring-bind (alias &optional (params nil))
                                           (cl-ppcre::split "\\?" alias :limit 2)
                                         (let ((alias (dc.utils:string->keyword alias)))
                                           (push (make-instance 'section
                                                                :alias alias
                                                                :params (decode-section-params params)
                                                                :template (make-instance 'basic-template
                                                                                         :markup content
                                                                                         )
                                                                )
                                                 (sections template))))
                                       "")
                                     :simple-calls t)))

;(defun count-plist-similarity (pl1 pl2)
;  (apply #'+ (mapcar

(defmethod section ((template sections-template) alias &key (params nil))
  (first (sort (remove alias (sections template)
                       :key #'alias
                       :test-not #'eql)
               #'< :key (lambda (section)
                          (if (equal params (params section))
                              0
                              (length (params section)))))))

;; List

(defclass list-template (sections-template)
  (
   ))

;(dc.utils:pizdec item-section)
(defgeneric item-section (list-template &key))

(defmethod item-section ((template list-template) &key item-number)
  (section template :item
           :params (cond
                     ((evenp item-number) (list :predicate "even"))
                     ((oddp item-number) (list :predicate "odd"))
                     (t nil)
                     )))

(defgeneric items-templates (list-template &optional list))

(defmethod items-templates ((template list-template) &optional (list nil))
  (mapcar #'template
          (loop for i from 1 upto (length list)
             collect (item-section template
                                   :item-number i))))

(defmethod view ((template list-template) &optional (list nil))
  (let* ((items-templates (items-templates template list))
         (items (mapcar (lambda (item-template record)
                          (view item-template record))
                        items-templates list)))
    (if (plusp (length items))
        (patch-slots (markup template)
                     (list :items (apply #'dc.utils:join items)))
        (view (template (section template :empty))))))

;; Menu

(defclass menu-template (list-template)
  (
   ))

(defmethod items-templates ((template menu-template) &optional (list nil))
  (mapcar #'template
          (loop for record in list
             collect (item-section template
                                   :is-current? (and (boundp 'hunchentoot::*request*)
                                                     (string= (hunchentoot::url-decode
                                                               (hunchentoot::request-uri hunchentoot::*request*))
                                                              (getf record :url)))))))

(defmethod item-section ((template menu-template) &key is-current?)
  (section template :item
           :params (if is-current?
                       (list :mode "current")
                       nil)))

;;; ~~~~~
;;; Views
;;; ~~~~~

;(defgeneric view (view))

;(defclass view ()
;  (
;   (template :initarg :template
;             :initform nil
;             :accessor template
;             )
;   ))

;(defclass data-view (view)
;  (
;   (data :initarg :data
;         :initform nil
;         :accessor data
;         )
;   ))

;(defmethod view ((view view))
;  (markup (template view)))

;(defclass view-template ()
;  (
;   (template :initarg :template
;             :initform nil
;             :accessor template
;             )
;   ))

;(defclass view-data (view-panel)
;  (
;   (sections :initarg :sections
;             :accessor sections
;             )
;   ))

;(defmethod build-view ((view view-panel) &key (data nil))
;  (declare (ignore data))
;  (template view))

;(defmethod compile-view ((view view-record) (data list))
;  view
;  data)
