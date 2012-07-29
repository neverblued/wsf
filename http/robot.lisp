;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defparameter robot-scanners
  (list :yandex (ppcre:create-scanner "yandex" :case-insensitive-mode t)
        :google (ppcre:create-scanner "googlebot" :case-insensitive-mode t)))

(defun robot? ()
  (true? (and (within-request-p)
              (iter (for (name scanner) in (group robot-scanners 2))
                    (when (ppcre:scan scanner (user-agent))
                      (leave t))))))
