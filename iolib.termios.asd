;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(asdf:defsystem :iolib.termios
  :description "Termios (3p) api wrappers"
  :maintainer "Razbegaev N.V. <marsijanin@gmail.com>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :iolib.streams :cffi :cffi-grovel)
  :components
  ((:file "pkgdcl")
   #+unix(cffi-grovel:grovel-file "ffi-termios-types-unix")
   #+unix(cffi-grovel:grovel-file "ffi-termios-constants")
   #+unix(:file "ffi-termios-functions-unix")
   #+unix(:file "wrapers")
   (:file "streams")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
