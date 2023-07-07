(asdf:defsystem :regrind-ee
  :depends-on (:hunchentoot :cl-who :one-more-re-nightmare :bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "workers")
               (:file "ui")))
