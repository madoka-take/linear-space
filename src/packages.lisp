(in-package :cl-user)

;; TODO: should be renamed
(defpackage :basic
  (:use :cl chiku.util))

(defpackage :scratch
  (:use :cl :chiku.util :basic))
