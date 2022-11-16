(in-package :cl-user)

;; TODO: should be renamed
(defpackage :basic.linear-space
  (:use :cl chiku.util))

(defpackage :scratch.linear-space
  (:use :cl :chiku.util :basic.linear-space))
