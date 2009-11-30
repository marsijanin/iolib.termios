;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; common lisp wrapers for termios (3) api - conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.termios)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition termios-error (iolib-error)
  ()
  (:documentation "Foundation of all iolib.termios conditions"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition termios-set-failled (termios-error)
  ((request :initarg :request :reader request))
  (:report
   (lambda (c s)
     (format s "Failled apply ~a request on serial device" (request c))))
  (:documentation
   "Signalled when `stty' failled to apply one of the requested settings"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition termios-set-baud-rate-failled (termios-set-failled)
  ()
  (:report
   (lambda (c s)
     (format s "Failled to setup ~a termios baud rate" (request c))))
  (:documentation "Signalled when requested baud rate is unsupported"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition termios-set-flag-failled (termios-set-failled)
  ()
  (:report
   (lambda (c s)
     (format s "Failled to setup ~a termios flag" (request c))))
  (:documentation "Signalled when requested flag is unsupported"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition termios-set-control-character-failled (termios-set-failled)
  ()
  (:report
   (lambda (c s)
     (format s "Failled to setup ~a termios control character" (request c))))
  (:documentation "Signalled when requested control character is unsupported"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
