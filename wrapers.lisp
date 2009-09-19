;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defparameter *cflags* '(cbaud exta extb csize cs5 cs6 cs7 cs8
			 cstopb cread parenb parodd hupcl clocal
			 loblk crtscts)   
  "Termios cflag filed constant")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *baud-rates* '(b0 b50 b75 b110 b134 b150 b200 b300
			     b600 b1200 b1800 b2400 b4800 b9600
			     b19200 b38400 b57600 b76800 b115200)
  "Baud rates constants")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *tcssetattr-actions* '(tcsanow tcsadrain tcsaflush)
  "Valid constants for tcsetattr action parameter")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *lflags* '(isig icanon xcase echo echoe echok echonl
			 noflsh iexten echoctl echoprt echoke flusho
			 pendin tostop)
  "Termios lflag filed constants")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *iflags* '(inpck ignpar parmrk istrip ixon ixoff
			 ixany ignbrk brkint inlcr igncr icrnl
			 iuclc imaxbel)
  "Termios iflag filed constants")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *oflags* '(opost olcuc onlcr ocrnl nocr onlret ofill
			 ofdel nldly nl0 nl1 crdly cr0 cr1 cr2 cr3
			 tabdly tab0 tab1 tab2 tab3 bsdly bs0 bs1
			 vtdly vt0 vt1 ffdly ff0 ff1)
  "Termios oflag filed constants")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *control-characters* '(vintr vquit verase vkill veof
				     veol veol2 vmin vtime)
  "Termios control character constants for termios cc field")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; </ Termios flags by fields >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-termios-option (termios flag-or-control-character &optional value)
  "Setup termios flag or control character. If `flag-or-control-character'
   is one of the termios flags (i.e. icanon) and no value is specified
   flag in corresponding termios field is reseted, otherwise seted.
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
			     :cc character)
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

		 