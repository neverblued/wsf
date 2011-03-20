(in-package #:blackjack)

;; generics

(defgeneric container (containable)
  (:documentation "Container of the item."))

(defgeneric contains (container)
  (:documentation "All items in the container."))

(defgeneric container-key (container)
  (:documentation "Container key function."))

(defgeneric container-test (container)
  (:documentation "Container test function."))

(defgeneric find-containing-key (container key-sample)
  (:documentation "Find item by it's key in the container."))

(defgeneric find-containing (container item)
  (:documentation "Find item in the container."))

(defgeneric put-into (containable container)
  (:documentation "Put the item into the container."))

(defgeneric take-out (containable container)
  (:documentation "Take the item out of the container."))

(defclass containable ()
  ((container :initarg :container :accessor container :initform nil)))

(defclass container ()
  ((container-list :accessor container-list :initform nil)
   (container-test :accessor container-test :initarg :test :initform #'eql)
   (container-key :accessor container-key :initarg :key :initform nil)))

;; methods

(defmethod find-containing-key ((container container) key-sample)
  (find key-sample
        (container-list container)
        :key (container-key container)
        :test (container-test container)))

(macrolet ((container-key-sample (container item)
             (with-gensyms (key* item*)
               `(let ((,key* (container-key ,container))
                      (,item* ,item))
                  (if ,key* (funcall ,key* ,item*) ,item*)))))

  (defmethod find-containing ((container container) (item containable))
    (find-containing-key container (container-key-sample container item)))

  (defmethod take-out ((item containable) (container container))
    (setf (container item) nil)
    (delete (container-key-sample container item)
            (container-list container)
            :key (container-key container)
            :test (container-test container))))

(defmethod put-into :around ((item containable) (container container))
  (if (eql container (container item))
      item
      (prognil (call-next-method))))

(defmethod put-into :before ((item containable) (container container))
  (let ((item-container (container item)))
    (when item-container
      (take-out item item-container))))

(defmethod put-into ((item containable) (container container))
  (push item (container-list container))
  (setf (container item) container))

(export '(container container-list container-key container-test containable find-containing find-containing-key put-into take-out))
