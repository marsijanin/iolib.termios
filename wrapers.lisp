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
    "Hash table of all valid termios options.
     Each entry is dot pair in (<option value> . <option designation>) form,
     i.e. (2 . lflag) etc.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defmacro define-termios-option (option filed)
    "Fill `*termios-options*' hash-table with valid termios options values."
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
  (let ((flag-value (gethash flag-or-control-character *termios-options*)))
    (cond
      ;; on of the {c,l,i,o}flag fields options:
      ((member (cdr flag-value) '(iflag oflag cflag lflag) :test #'eql)
       (setf (foreign-slot-value termios 'termios (cdr flag-value))
             ;; value specified => set (logior), reset over otherwise
             (funcall (if value #'logior #'logandc2)
                      (foreign-slot-value termios 'termios (cdr flag-value))
                      (car flag-value))))
      ;; control characters
      ((eql (cdr flag-value) 'control-chars)
       (setf (mem-aref (foreign-slot-pointer termios
                                             'termios
                                             'control-chars)
                       'cc
                       (car flag-value)) ;constant name is offset
             value))
      ;; baud-rates
      ((eql (cdr flag-value) 'baud-rates)
       (%cfsetispeed termios (car flag-value))
       (%cfsetospeed termios (car flag-value)))
      (t (error "Unknown termios option ~a" flag-or-control-character)))
    termios))
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
;; </ Termios options manipulation routines >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I guess that bits manipulation stuff is not a lisp way,
;; and using a kind of `stty (1p)' will be more lisp kind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun stty (fd &rest options)
  "Impliment stty (1p) in lisp way.
   Each `options' element should be termios option name
   or list in (option-name option-value) form.
   '(flag-name t) set corresponding flag,
   '(flag-name nil) or 'flag-name reset it.
   'baud-rate-constant set corresponding speed.
   '(control-character-name control-character-value) setup corresponding
    control character value.
   
   Setup for 8n1 mode: (stty fd '(evenp nil))
   Setup speed: (stty fd 'b115200)
   Setup raw mode: (stty fd 'raw) 
   Setup cooked mode: (stty fd '(raw nil) or (stty 'cooked)
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
	       (t (if (atom option)
		      (set-termios-option termios option)
		      (set-termios-option termios (first option)
					  (second option))))))
           (compare-termios (set test)
             (and
              (every #'(lambda (flag)
                         (=  (foreign-slot-value set  'termios flag)
                             (foreign-slot-value test 'termios flag)))
                     '(iflag oflag cflag lflag))
              (dotimes (i nccs t)
                (when
                    (/= (mem-aref (foreign-slot-pointer set
                                                        'termios
                                                        'control-chars)
                                  'cc
                                  i)
                        (mem-aref (foreign-slot-pointer test
                                                        'termios
                                                        'control-chars)
                                  'cc
                                  i))
                  (return nil))))))
    (with-foreign-objects ((set  'termios)
                           (test 'termios))
      (%tcgetattr fd set)
      ;; As said in man termios:
      ;; "tcsetattr() returns success if any of the
      ;; requested changes could be successfully carried out."
      ;; This manual also recomend to use tcgetattr() in oder to check
      ;; all performed settings. But I really dot't like to find back
      ;; that each zero ore one in coresponding field mead. So I prefer
      ;; to process each option step by step and singnal a condition
      ;; when there will be a difference:
      (dolist (option options)
	(process-option option set)
        (%tcsetattr fd tcsanow set)
        (%tcgetattr fd test)
        (or (compare-termios set test)
            (if (member option *baud-rates*)
                (error 'termios-speed-failled
                       :request (parse-integer (symbol-name option) :start 1))
                (error 'termios-set-failled
                       :request option)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
