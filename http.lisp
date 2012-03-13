;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

;; hunchentoot parameters

(setf *message-log-pathname*  (pathname "/home/lisp/log/message.log")
      *access-log-pathname*   (pathname "/home/lisp/log/access.log")
      *log-lisp-errors-p*     t
      *log-lisp-backtraces-p* t
      *approved-return-codes* (union *approved-return-codes*
                                     (list +http-not-found+
                                           +http-internal-server-error+))
      *handle-http-errors-p*  t
      *show-lisp-errors-p*    t
      ;*show-lisp-backtraces-p* t ; @bug: Undefined variable.
      )

;; misc

(defun yandex-bot? ()
  (true? (and (within-request-p)
              (ppcre:scan (ppcre:create-scanner "yandex"
                                                :case-insensitive-mode t)
                          (user-agent)))))

;; acceptors

(defvar acceptors nil)

(defun kill-acceptor (acceptor)
  (stop acceptor)
  (setf acceptors (delete acceptor acceptors)))

(defun acceptors (&key port domain)
  (remove-if-not (lambda (acceptor)
                   (and (or (null port)
                            (equal port (acceptor-port acceptor)))
                        (or (null domain)
                            (equal domain (acceptor-address acceptor)))))
                 acceptors))

(defun (setf acceptors) (new-list)
  (mapcar #'kill-acceptor (set-difference acceptors new-list))
  (setf acceptors new-list))

;; acceptor

(defgeneric accepting? (acceptor))
(defgeneric acceptor-server (acceptor))
(defgeneric (setf acceptor-server) (server acceptor))

(defclass acceptor (hunchentoot::acceptor)
  ((accepting? :reader accepting? :initform nil)
   (server :initarg :server :accessor acceptor-server :initform nil)))

(let ((handler #'handle-request))
  (fmakunbound 'handle-request)
  (defgeneric handle-request (acceptor request))
  (defmethod handle-request (acceptor request)
    (funcall handler acceptor request)))

(defmethod handle-request :around ((acceptor acceptor) request)
  (or (respond (acceptor-server acceptor) request)
      (call-next-method)))

(defmethod initialize-instance :after ((acceptor acceptor) &key)
  (pushnew acceptor acceptors)
  (start acceptor))

(defun stop&start (acceptor)
  (stop acceptor)
  (start acceptor))

(defmethod start :around ((acceptor acceptor))
  (unless (accepting? acceptor)
    (call-next-method)))

(defmethod start :after ((acceptor acceptor))
  (setf (slot-value acceptor 'accepting?) t)
  (signal 'server-on :server (acceptor-server acceptor)))

(defmethod stop :around ((acceptor acceptor))
  (when (accepting? acceptor)
    (call-next-method)))

(defmethod stop :after ((acceptor acceptor))
  (setf (slot-value acceptor 'accepting?) nil)
  (signal 'server-off :server (acceptor-server acceptor)))

;; server

(defparameter default-port 8666)

(defgeneric server-domain (server))
(defgeneric server-port (server))
(defgeneric server-acceptor (server))

(defclass http-server ()
  ((domain :initarg :domain :accessor server-domain :initform nil)
   (port :initarg :port :accessor server-port :initform default-port)
   (acceptor :accessor server-acceptor)))

(defmethod start ((server http-server))
  (start (server-acceptor server)))

(defmethod stop ((server http-server))
  (stop (server-acceptor server)))

(defun online? (http-server)
  (accepting? (server-acceptor http-server)))

(defun fetch-acceptor (http-server)
  (with-accessors ((port server-port)
                   (domain server-domain))
      http-server
    (or (let ((result (acceptors :domain domain :port port)))
          (awhen (first result) it))
        (make-instance 'acceptor
                       :server http-server
                       :address domain
                       :port port))))

(defmethod initialize-instance :after ((server http-server) &key)
  (setf (server-acceptor server)
        (fetch-acceptor server))
  (start server))

(defun throw-response (response)
  (throw 'response response))

(defmethod respond :around ((server http-server) request)
  (awith (catch 'response (call-next-method))
         (send (typecase it
                 (response it)
                 (string (make-instance 'text-response
                                        :content (join "Ответ сервера: " it)))
                 (t (default-response server request))))))

(defparameter slime-debug-conditions t)

(defmethod respond ((server http-server) (request request))
  (handler-case (call-next-method)
    ((or warning error wsf-condition)
        (condition)
      (if (and slime-debug-conditions
               (not (boundp '*reply*)))
          (invoke-debugger condition)
          (throw-response (failure-response server request condition))))))

(defmethod respond ((server http-server) (uri string))
  (let* ((*acceptor* (server-acceptor server))
         (*reply* (make-instance 'reply))
         (*request* (make-instance 'request :uri uri :acceptor *acceptor*)))
    (call-next-method)))

(defparameter default-http-content-format
  "<center><h1>~a</h1><big><p>~a</p><p>:( <big>&rarr;</big> <a href='/'>:)</a></p></big><center>")

(labels ((content (title message)
           (format nil default-http-content-format title message))

         (response (status title description)
           (make-instance 'html-response
                          :status status
                          :title title
                          :content (content title description))))

  (defmethod default-response ((server http-server) request)
    (response +http-not-found+
              "Страница не найдена"
              "Сервер не знает, как ответить."))

  (defmethod failure-response ((server http-server) request condition)
    (response +http-internal-server-error+
              "Отказ сервера"
              condition)))

;; debug

(defparameter last-request nil)

(defparameter last-requests nil)

(defmethod respond :before ((server http-server) (request request))
  (setf last-request request)
  (setf last-requests
        (cons last-request
              (if (> 5 (length last-requests))
                  last-requests
                  (subseq last-requests 0 4)))))

(defun last-request-uris ()
  (mapcar #'request-uri last-requests))

(defun test-last-requests (server)
  (mapcar (lambda (request)
            (respond server request))
          last-requests))
