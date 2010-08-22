(in-package #:dc.wsf)

;;; ~~~~~~~
;;; Service
;;; ~~~~~~~
                                        ; Classes
(defclass server ()
  ())

(defclass response ()
  (
   (content :initform :test-response
            :initarg :content
            :accessor content
            )
   ))
                                        ; Generics
(defgeneric respond (server &optional request))

(defgeneric send (response &key))

(defgeneric dispatcher (server))
                                        ; Methods
(defmethod respond ((server server) &optional (request nil))
  (declare (ignorable request))
  (send (make-instance 'response)))

(defmethod send ((response response) &key)
  (content response))

(defmethod dispatcher ((server server))
  (lambda (&optional request)
    (respond server request)))