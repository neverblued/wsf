(in-package #:wsf)

;; current specials

(defvar *route* nil "Текущий маршрут")

(defvar *routes* nil "Список пройденных маршрутов")

(defvar *router* nil "Текущий маршрутизатор")

;; route

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

;; router

(defclass router ()
  ((routes :accessor routes :initform nil)))

(defmacro with-router (router &body body)
  "Создать окружение маршрутизатора"
  `(let ((*router* ,router))
     (assert-type *router* 'router)
     ,@body))

(defmacro with-router-routes (&body body)
  `(let ((*routes* (routes *router*)))
     ,@body))

;; search

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
  (declare (ignore key))
  (true? (call-next-method)))

(defmethod route? (key)
  (route key))

(defmethod route? ((keys list))
  "Существуют ли все маршруты по списку ключей?"
  (every #'route keys))

(defun spare-routes ()
  "Список доступных маршрутов из текущего положения"
  (iter (for route in (routes *router*))
        (unless (route? route)
          (collect route))))

;; action

(defmacro with-route (route &body body)
  `(let* ((*route* ,route)
          (*routes* (adjoin *route* *routes*)))
     ,@body))

(defun call-route (route)
  (with-route route
    (funcall (route-action route))))

(defun may-follow? (route)
  (route? (route-follow route)))

(defun clause-fits? (route)
  (funcall (route-clause route)))

(defun find-next-route ()
  (iter (for route in (spare-routes))
        (when (and (may-follow? route)
                   (clause-fits? route))
          (leave route))))

(defun find-default-route ()
  (with-router-routes (route :default)))

(defun call-next-route ()
  (call-route (or (find-next-route)
                  (find-default-route)
                  (error 'route-not-found
                         :router *router*
                         :request *request*))))

;; pookies

(defvar *pookies* nil "Список текущих пуков")

(defclass pookie ()
  ((key :initarg :key :accessor pookie-key)
   (var :initarg :var :accessor pookie-var)
   (source :initarg :source :accessor pookie-source)
   (default :initarg :default :accessor pookie-default :initform nil)
   (encoder :initarg :encoder :accessor pookie-encoder :initform nil)))

(defun pookie-value (pookie)
  (or (funcall (pookie-source pookie))
      (funcall (pookie-default pookie))))

(defmacro with-pookie ((key var source &optional default encoder) &body body)
  (with-gensyms (^key^ ^pookie^)
    `(let* ((,^key^ (string-downcase (symbol-name ',key)))
            (,^pookie^ (make-instance 'pookie
                                      :key (symbol-keyword ',key)
                                      :var ',var
                                      :source (lambda ()
                                                (flet ((pookie-origin ()
                                                         (or (get-parameter ,^key^)
                                                             (post-parameter ,^key^))))
                                                  ,source))
                                      :default (lambda ()
                                                 ,default)
                                      :encoder (lambda (it)
                                                 (declare (ignorable it))
                                                 ,encoder)))
            (*pookies* (adjoin ,^pookie^ *pookies*))
            (,var (pookie-value ,^pookie^)))
       (declare (special ,var))
       ,@body)))

(defmacro with-pookies (list-of-pookies &body body)
  (let ((body `(progn ,@body)))
    (labels ((wrap-into-scope (list-of-pookies)
               (aif (first list-of-pookies)
                    `(with-pookie ,it
                       ,(aif (rest list-of-pookies)
                             (wrap-into-scope it)
                             body))
                    body)))
      (wrap-into-scope list-of-pookies))))

(defun pookie-current-value (pookie)
  (symbol-value (pookie-var pookie)))

(defun pookie-default-value (pookie)
  (funcall (pookie-default pookie)))

(defun pookie-encoded-value (pookie)
  (funcall (pookie-encoder pookie)
           (pookie-current-value pookie)))

(defun pookie-still-same? (pookie)
  (equal (pookie-current-value pookie)
         (pookie-default-value pookie)))

(defun pookies ()
  (iter (for pookie in *pookies*)
        (unless (pookie-still-same? pookie)
            (collect (pookie-key pookie))
            (collect (pookie-encoded-value pookie)))))

;; get parameters

(defun encode-get-parameters (parameters)
  (when parameters
    (apply #'join-by "&"
           (iter (for (key value) in (group parameters 2))
                 (collect (format nil "~(~a~)=~a" key value))))))

(defun decode-get-parameters (string)
  (unique-plist (iter (for pair in (ppcre:split "\\&" string))
                      (destructuring-bind (name &optional (value ""))
                          (ppcre:split "\\=" pair)
                        (collect (name-keyword name))
                        (collect value)))))

(defun insert-get-parameters (link parameters)
  (destructuring-bind (script &optional fragment)
      (bj:split-once "#" link)
    (let ((uri (if parameters
                   (destructuring-bind (script-name &optional script-args)
                       (bj:split-once "\\?" script)
                     (let* ((script-params (decode-get-parameters script-args))
                            (all-params (unique-plist (append parameters script-params)))
                            (string-params (encode-get-parameters all-params)))
                       (join script-name (aif string-params (join "?" it) ""))))
                   script)))
      (if fragment
          (join uri "#" fragment)
          uri))))

;; scope

(defvar *scope* nil "Текущее окружение")

(defmacro with-scope (scope &body body)
  `(let* ((*scope* (append ',scope *scope*))
          ,@scope)
     (declare (ignorable ,@(mapcar #'first scope)))
     ,@body))

;;; link

(defgeneric make-link (router route-key &rest args)
  (:documentation "Собрать ссылку"))

(defun link (route-key &rest parameters)
  (apply #'make-link *router* route-key parameters))

(defparameter broken-link "/404-broken-link/")

(defparameter broken-link? nil)

(defun broken-link ()
  (if broken-link? broken-link
      (let ((broken-link? t))
        (aif (find-default-route)
             (link it)
             (broken-link)))))

(defun route-args-scope (route)
  (awhen (route-args route)
    (iter (for key in it)
          (for pair = (find key *scope* :key #'first))
          (awhen pair
            (collect (symbol-keyword key))
            (collect (second it))))))

(defun route! (route)
  (typecase route
    (route route)
    (t (route route))))

(defmethod make-link (router route &rest parameters)
  (with-router router
    (aif (with-router-routes (route! route))
         (let ((link (apply (route-link it) parameters)))
           (aif (pookies)
                (insert-get-parameters link it)
                link))
         (broken-link))))

(defmacro set-special-link (route-key &body body)
  `(defmethod make-link :around ((router (eql *router*))
                                 (route (eql ,route-key))
                                 &rest parameters)
     (macrolet ((another-link (route-key)
                  `(apply #'make-link router ,route-key parameters)))
       (symbol-macrolet ((original-link (call-next-method)))
         ,@body))))

;; setup

(defmacro set-route (name &key (follow t) args link clause-with-scope scope clause pookies action)
  `(progn (setf (routes *router*)
                (delete ,name (routes *router*) :key #'route-name :test #'equal))
          (push (make-instance 'route
                               :name ,name
                               :follow ,follow
                               :args ',args
                               :link ,(aif link
                                           (if args
                                               `(lambda (&key ,@args &allow-other-keys)
                                                  (declare (ignorable ,@args))
                                                  ,it)
                                               `(lambda (&key)
                                                  ,it))
                                          `(lambda (&key)
                                             (broken-link)))
                               :clause (lambda ()
                                         ,(if (and (null clause) (null link))
                                              nil
                                              (let ((clause (or clause
                                                                (awith link
                                                                  (when (stringp it)
                                                                    `(string= ,it
                                                                              (script-name*)))))))
                                                (if clause-with-scope
                                                    `(with-scope ,scope ,clause)
                                                    clause))))
                               :action (lambda ()
                                         (with-scope ,scope
                                           (with-pookies ,pookies
                                             ,(or action '(call-next-route))))))
                (routes *router*))))

(defun unset-route (name)
  (setf (routes *router*)
        (delete name (routes *router*) :key #'route-name :test #'equal)))
