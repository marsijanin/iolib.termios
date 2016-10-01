;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

;;; Fix ecl long-long support
#+(and ecl (or cffi-features:no-long-long cffi-sys::no-long-long))
(defmethod perform :after ((o load-op) (c (eql (find-system :cffi))))
  (let* ((s (find-system :iolib.termios))
         (f (find-component s "ecl-long-long-fix")))
    (oos 'load-op f)))

(defsystem :iolib.termios
  :description "Termios (3p) api wrappers"
  :maintainer "Razbegaev N.V. <marsijanin@gmail.com>"
  :licence "MIT"
  :depends-on (:iolib/base :iolib/syscalls :iolib/streams :cffi :cffi-grovel :trivial-garbage)
  :components
  #+unix
  (#+ecl
   (:file "ecl-long-long-fix")
   (:file "pkgdcl")
   (cffi-grovel:grovel-file "ffi-termios-types-unix")
   (cffi-grovel:grovel-file "ffi-termios-constants-unix")
   (:file "ffi-termios-functions-unix")
   (:file "conditions")
   (:file "wrappers")
   (:file "streams")))
;;;; EOF
