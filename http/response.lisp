;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defmethod send :before (response)
  (if (within-headless-reply?)
      (format t "~&WSF headless response # ~a~&~%" (status response))
      (set-reply response)))

(defun string-response (content)
  (make-instance 'text-response :content (join "Ответ сервера: " content)))

(labels ((content (title message)
           (format nil default-http-content-format
                   title message))

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
