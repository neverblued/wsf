(in-package #:wsf)

(defgeneric site-docroot (site)
  (:documentation "Site docroot folder path."))

(defgeneric from-docroot (site relative-path)
  (:documentation "Merge SITE docroot with RELATIVE-PATH."))

(defmethod from-docroot (site relative-path)
  (join (site-docroot site) "/" relative-path))
