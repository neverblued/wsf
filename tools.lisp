;; (c) Дмитрий Пинский <demetrius@neverblued.info>
;; Допускаю использование и распространение согласно
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(defmacro push-trim (item list &key limit)
  (once-only (limit)
    `(setf ,list
           (cons ,item
                 (awith ,list
                   (if (> ,limit (length it))
                       it
                       (subseq it 0 (1- ,limit))))))))
