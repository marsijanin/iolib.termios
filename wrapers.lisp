;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; common lisp wrapers for termios (3) api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.termios)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <Termios options manipulation routines>
(defmacro membercase (form &rest clauses)
  "(membercase 'a 
	    ('(b c e) 1)
	    ('(e d b) 2)
	    ('(a b c) 3))
    ==> 3
  "
  `(cond ,@(mapcar #'(lambda (x)
                       (let ((lst (first x))
                             (body (rest x)))
                         `((member ,form ,lst) ,@body)))
                   clauses)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro enumcase (form &rest clauses)
  "(enumcase :b115200
		   (baud-rates 1)
		   (iflags 2)
   		   (cflags 3))
   ==> 1
  "
  `(membercase ,form ,@(mapcar #'(lambda (x)
                                   (let ((enum (first x))
                                         (body (rest x)))
                                     `((foreign-enum-keyword-list ',enum)
                                       ,@body)))
                               clauses)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun which-termios-keyword (keyword)
  (enumcase keyword
   (iflag 'iflag)
   (oflag 'oflag)
   (lflag 'lflag)
   (cflag 'cflag)
   (control-character 'control-character)
   (baud-rate 'baud-rate)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun termios-flag-p (keyword)
  (member (which-termios-keyword keyword) '(iflag oflag cflag lflag)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun termios-control-character-p (keyword)
  (eql 'control-character (which-termios-keyword keyword)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun termios-baud-rate-p (keyword)
  (eql 'baud-rate (which-termios-keyword keyword)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setup-termios-flag (termios flag &optional setp)
  "Reset `flag' in corresponding `termios' field, or set in, if `setp' is true."
  (let ((type (which-termios-keyword flag)))
    (unless type
      (error "Unknown termios option ~a" flag))
    (setf (foreign-slot-value termios 'termios type)
            ;; value specified => set (logior), reset over otherwise
            (funcall (if setp #'logior #'logandc2)
                     (foreign-slot-value termios 'termios type)
                     (foreign-enum-value type flag)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setup-termios-control-character (termios cc value)
  "Setup corresponding control character value.
   Value can be `control-character' keyword or integer."
  (setf (mem-aref (foreign-slot-pointer termios
                                        'termios
                                        'control-chars)
                       'cc
                       ;; constant name is offset
                       (foreign-enum-value 'control-character cc)) 
        (if (integerp value)
            value
            (foreign-enum-value 'control-character value))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-raw-termios (termios)
  "Same effect as cfmakeraw()"
  (dolist (flag
            '(:ignbrk :brkint :parmrk :istrip :inlcr :igncr :icrnl :ixon ;iflag
              :opost						         ;oflag
              :echo :echonl :icanon :isig :iexten		         ;lflag
              :csize :parenb))				                 ;cflag
    (setup-termios-flag termios flag))
  (setup-termios-flag termios :cs8 t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-cooked-termios (termios)
  "Effect is opposite to `make-raw-termios'"
  (dolist (flag
            '(:ignbrk :brkint :parmrk :istrip :inlcr :igncr :icrnl :ixon ;iflag
              :opost						         ;oflag
              :echo :echonl :icanon :isig :iexten	                 ;lflag
              :csize :parenb))				                 ;cflag
    (setup-termios-flag termios flag t))
  #|(set-termios-option termios 'cs8 t)|#)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-evenp-termios (termios)
  "Enable parenb and cs7; disable parodd"
  (setup-termios-flag termios :parenb t)
  (setup-termios-flag termios :parodd)
  (setup-termios-flag termios :cstopb)
  (setup-termios-flag termios :csize)
  (setup-termios-flag termios :cs7 t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-oddp-termios (termios)
  "Enable parenb, cs7, and parodd"
  (setup-termios-flag termios :parenb t)
  (setup-termios-flag termios :parodd t)
  (setup-termios-flag termios :cstopb)
  (setup-termios-flag termios :csize)
  (setup-termios-flag termios :cs7 t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-8n1-termios (termios)
  "Disable parenb, and set cs8."
  (setup-termios-flag termios :parenb)
  (setup-termios-flag termios :cstopb)
  (setup-termios-flag termios :csize)
  (setup-termios-flag termios :cs8 t))
;; </ Termios options manipulation routines >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I guess that bits manipulation stuff is not a lisp way,
;; and using a kind of `stty (1p)' will be more lisp kind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun stty (serial &rest options)
  "Impliment stty (1) in lisp way.
   `serial` can be stream or fd.
   `options` should be list of termios keywords and values:
   `nil` or `t` for flags or one of `:raw`, `:cooked`, `:evenp` or `:oddp`,
   integers for control characters.
  
   `:inlcr t` set corresponding flag,
   `:inlcr nil` reset it,
   `:b115200` set corresponding speed,
   `:vtime 0` setup corresponding control character value.
   
   Setup for 8n1 mode: `(stty fd :evenp nil)`
   Setup speed: `(stty fd :b115200)` or `(stty my-stream 115200)`
   Setup raw mode and speed: `(stty fd :b115200 :raw t)` 
   Setup cooked mode: `(stty fd :raw nil)`
  "
  (labels
      ;; As said in man termios:
      ;; >tcsetattr() returns success if any of the
      ;; >requested changes could be successfully carried out."
      ;; This manual also recomend to use tcgetattr() in oder to check
      ;; all performed settings. But I really dot't like to find back
      ;; that each zero ore one in coresponding field mead. So I prefer
      ;; to process each option step by step and signal a condition
      ;; when there will be a difference:
      ((%set-test-termios (fd set test)
         (%tcsetattr fd :tcsanow set)
         (%tcgetattr fd test)
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
              (return nil)))))
       (%stty (set test fd options)
         (let ((opt  (car options))
               (setp (cadr options))
               (rst  (cddr options)))
           (cond
             ((termios-flag-p opt)
              (setup-termios-flag set opt setp)
              (unless (%set-test-termios fd set test)
                (error 'termios-set-flag-failled :request opt)))
             ((termios-control-character-p opt)
              (setup-termios-control-character set opt setp)
              (unless (%set-test-termios fd set test)
                (error 'termios-set-control-character-failled :request opt)))
             ((eql opt :raw)
              (if setp (make-raw-termios set) (make-cooked-termios set))
              (unless (%set-test-termios fd set test)
                (error 'termios-set-failled
                        :request (if setp :raw :cooked))))
             ((eql opt :cooked)
              (if setp (make-cooked-termios set) (make-raw-termios set))
              (unless (%set-test-termios fd set test)
                (error 'termios-set-failled
                        :request (if setp :cooked :raw))))
             ((eql opt :evenp)
              (if setp (make-evenp-termios set) (make-oddp-termios set))
              (unless (%set-test-termios fd set test)
                (error 'termios-set-failled
                        :request (if setp :evenp :oddp))))
             ((eql opt :oddp)
              (if setp (make-oddp-termios set) (make-evenp-termios set))
              (unless (%set-test-termios fd set test)
                (error 'termios-set-failled
                        :request (if setp :oddp :evenp)))))
           (if rst
               (%stty set test fd rst)
               set))))
    (let ((fd (typecase serial
                (integer serial)
                (stream (fd-of serial))
                (t (error "You should specify stream or fd"))))
          (opts-w/o-baud (remove-if #'termios-baud-rate-p options))
          (baud  (find-if #'termios-baud-rate-p options)))
      (when (and options (oddp (length opts-w/o-baud)))
        (error "Mailformed stty options list"))
      (with-foreign-objects ((set  'termios)
                             (test 'termios))
        (%tcgetattr fd set)
        (when baud
          (%cfsetispeed set baud)
          (%cfsetospeed set baud)
          (%tcsetattr fd :tcsanow set)
          (%tcgetattr fd test)
          (let ((ispeed (%cfgetispeed test))
                (ospeed (%cfgetospeed test)))
            (unless (and (eql baud ispeed) (eql baud ospeed))
              (error 'termios-set-baud-rate-failled :request baud))))
        (%stty set test fd opts-w/o-baud)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
