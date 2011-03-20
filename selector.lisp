(in-package #:blackjack)

(defgeneric options (selector))
(defgeneric select (selector &optional request))
(defgeneric find-option (selector request))
(defgeneric selection-predicate (option request))
(defgeneric succeed-selection (selector request option))
(defgeneric fail-selection (selector request))

(defmethod select (selector &optional request)
    (aif (find-option selector request)
         (succeed-selection selector request it)
         (fail-selection selector request)))

(defmethod find-option :around (selector request)
  (car (call-next-method)))

(defmethod find-option (selector request)
  (remove-if-not (lambda (option)
                   (selection-predicate option request))
                 (options selector)))

(defmethod selection-predicate (option request)
  (declare (ignore option request))
  nil)

(defmethod succeed-selection (selector request option)
  (declare (ignore selector request))
  option)

(define-condition option-not-found ()
  ((selection-selector :initarg :selector :reader selection-selector)
   (selection-request :initarg :request :reader selection-request)))

(defmethod fail-selection :before (selector request)
  (signal (make-condition 'option-not-found :selector selector :request request)))

(defmethod fail-selection (selector request)
  nil)

(export '(options select selection-predicate succeed-selection fail-selection option-not-found))
