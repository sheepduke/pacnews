(defpackage :pacnews
  (:use :cl :alexandria)
  (:import-from :drakma
                :http-request)
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings
                :regex-replace-all)
  (:export :news))

(in-package :pacnews)
