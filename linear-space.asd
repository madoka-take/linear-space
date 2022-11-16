(defsystem :linear-space
  :version "0.1.0"
  :maintainer "Madoka Take"
  :author "Madoka Take"
  :license "MIT License"
  :description "Code pieces that help manual calculation in linear spaces."
  :long-description "Common Lisp code pieces that help manual calculation in linear spaces."
  :serial t
  :depends-on (:chiku.util)
  :components ((:file "src/packages")
               (:file "src/basic")))