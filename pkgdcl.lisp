;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; termios (3p) api wrapers -  package definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :iolib.termios
  (:nicknames #:itty)
  (:use :iolib.base :iolib.streams :cffi)
  (:import-from :iolib.syscalls :defsyscall :defcfun*)
  (:export
   ;; Struct termios:
   #:termios
   ;; and settings constantenums
   #:iflag
   #:oflag
   #:cflag
   #:lflag
   #:control-character
   #:baud-rate
   ;; termios.h (0p) functions:
   #:%cfgetispeed
   #:%cfgetospeed
   #:%cfsetispeed
   #:%cfsetospeed
   #:%tcdrain
   #:%tcflow
   #:%tcflush
   #:%tcgetattr
   #:%tcgetsid
   #:%tcsendbreak
   #:%tcsetattr
   ;; classes:
   #:dual-channel-tty-gray-stream
   ;; serial devices streams timeout accessors:
   #:read-timeout
   #:write-timeout
   ;; useful wrapers:
   #:stty
   ;; coditions:
   #:termios-error
   #:termios-set-failled
   #:termios-set-flag-failled
   #:termios-set-control-character-failled
   #:termios-set-baud-rate-failled
   ;; serila stream manipulation
   #:open-serial-stream
   #:with-serial-stream
   #:with-serial-streams
   ;; List of all open serial streams. Can be used in signal handlers.
   #:*open-serial-streams*))            ;</ defpackage>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
