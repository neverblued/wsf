(in-package #:wsf)

(defparameter auth-cookie-name "authentication-checksum")

(defparameter max-auth-cookie-life (* 365 24 60 60))

(defvar auth-cookie-life (make-hash-table))

(defun auth-cookie-life (subject)
  (gethash subject auth-cookie-life max-auth-cookie-life))

(defun (setf auth-cookie-life) (new-life subject)
  (setf (gethash subject auth-cookie-life)
        (or new-life max-auth-cookie-life)))

(defun auth-period (subject)
  (reduce #'min (mapcar #'auth-cookie-life (kgb::all-groups subject))))

(defun set-auth-cookie (user)
  (set-cookie auth-cookie-name
              :value (kgb::user-ausweis user)
              :expires (+ (get-universal-time) (auth-period user))
              :path "/"))

(defun authenticate-cookie ()
  (let ((cookie (cookie-out auth-cookie-name)))
    (when cookie
      (kgb::find-user-ausweis cookie))))

(defmethod authenticate ((request request))
  (start-session)
  (authenticate-cookie))
