(in-package :cl-user)

(defpackage :utility.linear-space
  (:use :cl :chiku.util))

;; TODO: should be renamed
(defpackage :basic.linear-space
  (:use :cl chiku.util :utility.linear-space))

(defpackage :scratch.linear-space
  (:use :cl :chiku.util :utility.linear-space :basic.linear-space))
