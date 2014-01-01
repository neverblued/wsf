;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package #:wsf)

(setf ;*message-log-pathname*  (pathname "/home/lisp/log/message.log")
      ;*access-log-pathname*   (pathname "/home/lisp/log/access.log")
      *log-lisp-errors-p*      t
      *log-lisp-backtraces-p*  t
      *show-lisp-errors-p*     t
      *show-lisp-backtraces-p* t
      )

(defparameter default-port 8666)

(defparameter slime-debug-conditions
  t)

(defparameter default-http-content-format
  "<center><h1>~a</h1><big><p>~a</p><p>:( <big>&rarr;</big> <a href='/'>:)</a></p></big><center>")
