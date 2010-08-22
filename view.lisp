(defpackage #:dc.wsf.view
  (:use
     #:common-lisp
     #:cl-ppcre
     )
  (:export
     #:basic-template
     #:list-template
     #:template.load
     #:view
     )
  )

(in-package #:dc.wsf.view)

;;; ~~~~~~~~~
;;; Templates
;;; ~~~~~~~~~

(defparameter *patch-section-slot-format* "{{slot:patch-section=~a}}")
(defparameter *slot-regex* "\\{\\{slot:([^\\}]+)\\}\\}")
(defparameter *section-regex*
  (cl-ppcre::create-scanner '(:sequence "{{section:"
                                        (:register (:greedy-repetition 0 nil (:inverted-char-class #\})))
                                        "}}"
                                        (:regex "\\s*")
                                        (:register (:non-greedy-repetition 0 nil :everything))
                                        (:regex "\\s*")
                                        "{{/section}}"
                              )
                            :single-line-mode t))
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

(defclass basic-template ()
  (
   (markup :initform ""
           :initarg :markup
           :accessor markup)
   ))

(defclass sections-template (basic-template)
  (
   (sections :initform nil
             :accessor sections)
   ))

(defclass list-template (sections-template)
  (
   ))

(defmethod initialize-instance :after ((template basic-template) &key)
  (setf (markup template)
        (cl-ppcre::regex-replace-all *comment-regex*
                                     (markup template)
                                     "")))

(defmethod initialize-instance :after ((template sections-template) &key)
  (setf (markup template)
        (cl-ppcre::regex-replace-all *section-regex*
                                     (markup template)
                                     (lambda (match alias content &rest registers)
                                       (declare (ignorable match alias content registers))
                                       (setf (getf (sections template)
                                                   (dc.utils:string->keyword alias))
                                             content)
                                       (format nil *patch-section-slot-format* alias))
                                     :simple-calls t)))

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

(defgeneric view (template &optional data))

(defmethod view ((template basic-template) &optional (data nil))
  (patch-slots (markup template) data))

(defmethod view ((template list-template) &optional (list nil))
  (let ((items (loop
                   for record in list
                   collect (patch-slots (getf (sections template) :item)
                                        record))))
    (if (plusp (length items))
        (patch-slots (markup template)
                     (list :patch-section=item (apply #'dc.utils:join items)
                           :patch-section=empty ""))
        (getf (sections template) :empty))))

;(defun parse-template-sections (source)
;  (let* ((sections nil)
;         (canvas (cl-ppcre::regex-replace-all *regex-section*
;                                              source
;                                              (lambda (match alias content &rest registers)
;                                                (declare (ignorable match alias content registers))
;                                                (setf (getf sections (dc.utils:string->keyword alias)) content)
;                                                (format nil "{ins:section=~a}" alias))
;                                              :simple-calls t
;                                              )))
;    (make-instance 'template-sections
;                   :markup canvas
;                   :sections sections)))

;(defun view-list (template list)
;  (let* (
;         (list-template (parse-template-sections template))
;         (items (loop
;                   for record in list
;                   collect (view-record (getf (sections list-template) :item)
;                                        record)))
;         )
;    (if (plusp (length items))
;        (view-record (markup list-template)
;                     (list :section=item (apply #'dc.utils:join items)
;                           :section=empty ""))
;        (getf (sections list-template) :empty))))

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
