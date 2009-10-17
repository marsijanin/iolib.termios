;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; common lisp wrapers for termios (3) api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.termios)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sort flags by termios fileds:
;; (as described in ,,Posix serial programming manual``)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't using defconstant case sbcl singnal constant redefining error
;; with and w/o eval-when case results are not eql
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *cflags* '(cbaud cbaudex csize cs5 cs6 cs7 cs8 cstopb cread
                           parenb parodd hupcl clocal loblk cibaud cmspar
                           crtscts)
    "Termios cflag filed constant")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defparameter *baud-rates* '(b0 b50 b75 b110 b134 b150 b200 b300 b600 b1200
                               b1800 b2400 b4800 b9600 b19200 b38400 b57600
                               b115200 b230400 b460800 b500000 b576000 b921600
                               b1000000 b1152000 b1500000 b2000000 b2500000
                               b3000000 b3500000 b4000000)
    "Baud rates constants")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defparameter *tcssetattr-actions* '(tcsanow tcsadrain tcsaflush)
    "Valid constants for tcsetattr action parameter")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defparameter *lflags* '(isig icanon xcase echo echoe echok echonl echoctl
                           echoprt echoke defecho flusho noflsh tostop pendin
                           iexten)
    "Termios lflag filed constants")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defparameter *iflags* '(ignbrk brkint ignpar parmrk inpck istrip inlcr igncr
                           icrnl iuclc ixon ixany ixoff imaxbel iutf8)
    "Termios iflag filed constants")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defparameter *oflags* '(opost olcuc onlcr ocrnl onocr onlret ofill ofdel
                           nldly crdly tabdly bsdly vtdly ffdly)
    "Termios oflag filed constants")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defparameter *control-characters* '(vintr vquit verase vkill veof vmin veol
                                       vtime veol2 vswtch vstart vstop vsusp
                                       vdsusp vlnext vwerase vreprint vdiscard
                                       vstatus)
    "Termios control character constants for termios cc field")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defparameter *termios-options* (make-hash-table :test #'eql)
    "Hash table of all valid termios options.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defmacro define-termios-option (option filed)
    "Fill `*termios-options*' hash-table with valid termios optins values."
    `(when (boundp ,option)
       (setf (gethash ,option *termios-options*)
             (cons (symbol-value ,option) ',filed))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Filling *termios-options*

  (mapcar #'(lambda (x) (define-termios-option x iflag)) *iflags*)
  (mapcar #'(lambda (x) (define-termios-option x oflag)) *oflags*)
  (mapcar #'(lambda (x) (define-termios-option x lflag)) *lflags*)
  (mapcar #'(lambda (x) (define-termios-option x cflag)) *cflags*)
  (mapcar #'(lambda (x) (define-termios-option x control-chars))
          *control-characters*)
  (mapcar #'(lambda (x) (define-termios-option x baud-rates))
          *baud-rates*))                ;</ (eval-when
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; </ Termios flags by fields >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < Termios options manipulation routines >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-termios-option (termios flag-or-control-character &optional value)
  "Setup termios flag or control character. If `flag-or-control-character'
   is one of the termios flags (i.e. icanon) and `value' is not specified
   then flag in corresponding termios field is reseted, otherwise seted.
   If  `flag-or-control-character' is control character corresponding
   value in termios cc field is seted to the `value' parameter.

   Reset icanon flag: (set-termios-option term 'icanon)
   Set   icanon flag: (set-termios-option term 'icanon t)
   Set vtime value to 0: (set-termios-option 'vtime 0)
  "
  (labels ((set-flag (flag field)
	     (setf (foreign-slot-value termios 'termios field)
		   (logior flag (foreign-slot-value termios 'termios field))))
	   (reset-flag (flag field)
	     (setf (foreign-slot-value termios 'termios field)
		   (logandc2 flag (foreign-slot-value termios 'termios field))))
	   (set-control-character (character value)
	     (setf (mem-aref (foreign-slot-pointer termios 'termios 'control-chars)
			     'cc character)
		   value)))
    (let ((fvalue (symbol-value flag-or-control-character)))
      (cond ((member flag-or-control-character *cflags*)
	     (if value
		 (set-flag fvalue 'cflag)
		 (reset-flag fvalue 'cflag)))
	    ((member flag-or-control-character *lflags*)
	     (if value
		 (set-flag fvalue 'lflag)
		 (reset-flag fvalue 'lflag)))
	    ((member flag-or-control-character *iflags*)
	     (if value
		 (set-flag fvalue 'iflag)
		 (reset-flag fvalue 'iflag)))
	    ((member flag-or-control-character *oflags*)
	     (if value
		 (set-flag fvalue 'oflag)
		 (reset-flag fvalue 'oflag)))
	    ((member flag-or-control-character *control-characters*)
	     (if value
		 (set-control-character fvalue value)
		 (error "You should specify control character value")))
	    (t (error "Unknown termios flag or control character ~a"
		      flag-or-control-character))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-raw-termios (termios)
  "Same effect as cfmakeraw()"
  (dolist (flag '(ignbrk brkint parmrk istrip inlcr igncr icrnl ixon ;iflag
		  opost						     ;oflag
		  echo echonl icanon isig iexten		     ;lflag
		  csize parenb))				     ;cflag
    (set-termios-option termios flag))
  (set-termios-option termios 'cs8 t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-cooked-termios (termios)
  "Effect is opposite to `make-raw-termios'"
  (dolist (flag '(ignbrk brkint parmrk istrip inlcr igncr icrnl ixon ;iflag
		  opost						     ;oflag
		  echo echonl icanon isig iexten		     ;lflag
		  csize parenb))				     ;cflag
    (set-termios-option termios flag t))
  #|(set-termios-option termios 'cs8 t)|#)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-evenp-termios (termios)
  "Enable parenb and cs7; disable parodd"
  (set-termios-option termios 'parenb t)
  (set-termios-option termios 'parodd)
  (set-termios-option termios 'cstopb)
  (set-termios-option termios 'csize)
  (set-termios-option termios 'cs7 t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-oddp-termios (termios)
  "Enable parenb, cs7, and parodd"
  (set-termios-option termios 'parenb t)
  (set-termios-option termios 'parodd t)
  (set-termios-option termios 'cstopb)
  (set-termios-option termios 'csize)
  (set-termios-option termios 'cs7 t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-8n1-termios (termios)
  "Disable parenb, and set cs8."
  (set-termios-option termios 'parenb)
  (set-termios-option termios 'cstopb)
  (set-termios-option termios 'csize)
  (set-termios-option termios 'cs8 t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-really-raw-termios (termios)
  "Leave cread and clocal flags setted, reset others"
  (dolist (neet-be-set '(cread clocal))
    (set-termios-option termios neet-be-set t))
  (dolist (dont-need-for-with-stuff
            (append (remove-if #'(lambda (x)
                                   (member x '(cread clocal)))
                               *cflags*)
                    *iflags* *oflags* *lflags*))
    (set-termios-option termios dont-need-for-with-stuff))
  (dolist (control-character *control-characters*)
    (set-termios-option termios control-character 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; </ Termios options manipulation routines >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I guess that bits manipulation stuff is not a lisp way,
;; and using a kind of `stty (1p)' will be more lisp kind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun stty (fd &rest options)
  "Impliment stty (1p) in lisp way.
   Each `options' elemen should be termios option name
   or list in (option-name option-value) form.
   '(flag-name t) set corresponding flag,
   '(flag-name nil) or 'flag-name reset it.
   'baud-rate-constant set corresponding speed.
   '(control-character-name control-character-value) setup corresponding
    control character value.
   
   Setup for 8n1 mode: (stty fd '(evenp nil))
   Setup speed: (stty fd 'b115200)
   Setup raw mode: (stty fd 'raw) or (stty fd '(raw t))
   Setup cooked mode: (stty fd '(raw nil) or (stty 'cooked)
   Reset all except cread and clocal, set 115200-8n1 mode:
   (stty fd 'really-raw '(parity nil) 'b115200)
  "
  (labels ((process-option (option termios)
	     (cond 
	       ((member option *baud-rates*)
		(%cfsetispeed termios (symbol-value option))
		(%cfsetospeed termios (symbol-value option)))
	       ((member option '(evenp 'parity))
		(make-evenp-termios termios))
	       ((eq option 'oddp)
		(make-oddp-termios termios))
	       ((and (consp option)
		     (null (second option))
		     (member (first option)
			     '(parity  evenp oddp)))
		(make-8n1-termios termios))
	       ((eql option 'raw)
		(make-raw-termios termios))
	       ((or (eql option 'cooked)
		    (and (consp option)
			 (eql (first option) 'raw)
			 (null (second option))))
		(make-cooked-termios termios))
               ((eql option 'really-raw)
                (make-really-raw-termios termios))
	       (t (if (atom option)
		      (set-termios-option termios option)
		      (set-termios-option termios (first option)
					  (second option)))))))
    (with-foreign-object (ptr 'termios)
      (%tcgetattr fd ptr)
      (dolist (option options)
	(process-option option ptr))
      (%tcsetattr fd tcsanow ptr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
