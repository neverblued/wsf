(in-package #:wsf)

(defmacro with-db (database &body body)
  `(aif ,database
        (with-connection (list it postgrace::user (postgrace::password) postgrace::host)
          ,@body)
        (progn ,@body)))

(defmacro with-site-db (site &body body)
  `(with-db (site-database ,site)
     ,@body))
