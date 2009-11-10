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
     (declare (ignorable c))
     (format s "Failled apply ~a request on serial device" (request c))))
  (:documentation
   "Signalled when `stty' failled to apply one of the requested settings"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition termios-speed-failled (termios-set-failled)
  ()
  (:report
   (lambda (c s)
     (declare (ignorable c))
     (format s "Failled to setup ~a baud speed on serial device" (request c))))
  (:documentation "Signalled by `stty' when requested speed is unsupported"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
