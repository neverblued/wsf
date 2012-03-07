;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defclass route ()
  ((name :initarg :name :accessor route-name)
   (follow :initarg :follow :accessor route-follow)
   (args :initarg :args :accessor route-args :initform nil)
   (clause :initarg :clause :accessor route-clause)
   (action :initarg :action :accessor route-action)
   (link :initarg :link :accessor route-link)))

(defmethod print-object ((route route) stream)
  (format stream "#<ROUTE:~a>" (route-name route)))

(defgeneric route (key)
  (:documentation "Искать маршрут по ключу в текущем списке."))

(defvar *routes* nil "Список пройденных маршрутов")

(defmethod route ((key route))
  "Искать данный маршрут"
  (find key *routes*))

(defmethod route ((key symbol))
  "Искать по имени в виде ключевого слова"
  (find key *routes* :key #'route-name :test #'equal))

(defmethod route ((key string))
  "Искать по имени в виде строки"
  (route (name-keyword key)))

(defmethod route ((keys list))
  "Список результатов поиска маршрутов по очереди списка ключей"
  (remove nil (mapcar #'route keys)))

(defmethod route ((key (eql t)))
  "T такое Т"
  t)

(defgeneric route? (key)
  (:documentation "Существует ли маршрут по ключу в списке?"))

(defmethod route? :around (key)
  (true? (call-next-method)))

(defmethod route? (key)
  (route key))

(defmethod route? ((keys list))
  "Существуют ли все маршруты по списку ключей?"
  (every #'route keys))

(defun route! (route)
  "Убедиться, что аргумент это ROUTE."
  (typecase route
    (route route)
    (t (route route))))
