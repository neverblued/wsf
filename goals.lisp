;; (c) www.neverblued.info
;; LLGPL -> http://opensource.franz.com/preamble.html

(in-package :wsf)

(defclass goal ()
  ((alias :initarg :alias :accessor goal-alias :initform (error "need alias"))
   (link :initarg :link :accessor goal-link :initform (error "need link"))
   (referer :initarg :referer :accessor goal-referer)
   (time :initarg :time :accessor goal-time :initform (get-universal-time))
   (user :initarg :user :accessor goal-user :initform kgb::user)
   (ip   :initarg :ip   :accessor goal-ip   :initform (real-remote-addr))))

(defun serialize-goal (goal)
  (list :alias (goal-alias goal)
        :link (goal-link goal)
        :referer (goal-referer goal)
        :time (goal-time goal)
        :user (kgb::alias (goal-user goal))
        :ip (goal-ip goal)))

(defparameter goals-storage-pathname "log/goals.log")

(defun achieve (site goal)
  (let ((storage (from-docroot site goals-storage-pathname)))
    (save-into-file (adjoin (serialize-goal goal)
                            (load-from-file storage))
                    storage))
  nil)

(defun goals (site)
  (let ((storage (from-docroot site goals-storage-pathname)))
    (mapcar (lambda (data)
              (make-instance 'goal
                             :alias (getf data :alias)
                             :link (getf data :link)
                             :referer (getf data :referer)
                             :time (getf data :time)
                             :user (kgb::alias (getf data :user))
                             :ip (getf data :ip)))
            (load-from-file storage))))
