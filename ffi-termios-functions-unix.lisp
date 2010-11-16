;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;; Termios (3p) api wrappers for iolib - foreign functions

(in-package :iolib.serial)

(defcfun* (%cfgetispeed "cfgetispeed") baud-rate
  (termios :pointer))			; const struct termios *

(defcfun* (%cfgetospeed "cfgetospeed") baud-rate
  (termios :pointer))			; const struct termios *

(defsyscall (%cfsetispeed "cfsetispeed") :int
  (termios :pointer)			; struct termios *
  (speed   baud-rate))

(defsyscall (%cfsetospeed "cfsetospeed") :int
  (termios :pointer) 			; struct termios *	
  (speed   baud-rate))

(defsyscall (%tcdrain "tcdrain") :int
  (fd :int))

(defsyscall (%tcflow "tcflow")   (:int :restart t :handle fd)
  (fd     :int)
  (action :int))

(defsyscall (%tcflush "tcflush") (:int :restart t :handle fd)
  (queue-selector :int)
  (fd             :int))

(defsyscall (%tcgetattr "tcgetattr") (:int :restart t :handle fd)
  (fd      :int)
  (termios :pointer)) 			; const struct termios *

(defsyscall (%tcgetsid "tcgetsid") isys:pid-t
  (fd :int))

(defsyscall (%tcsendbreak "tcsendbreak") (:int :restart t :handle fd)
  (fd       :int)
  (duration :int))

(defsyscall (%tcsetattr "tcsetattr") (:int :restart t :handle fd)
  (fd             :int)
  (optional-ation tcssetattr-action)
  (termios        :pointer))
;;;; EOF
