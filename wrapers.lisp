;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; common lisp wrapers for termios (3) api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.termios)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sort flags by termios fileds:
;; (as described in ,,Posix serial programming manual``)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconstant +cflags+ (list :cbaud :exta :extb :csize :cs5 :cs6 :cs7 :cs8
			      :cstopb :cread :parenb :parodd :hupcl :clocal
			      :loblk :crtscts)   
    "Termios cflag filed constant")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconstant +baud-rates+ (list :b0 :b50 :b75 :b110 :b134 :b150 :b200 :b300
				  :b600 :b1200 :b1800 :b2400 :b4800 :b9600
				  :b19200 :b38400 :b57600 :b76800 :b115200)
    "Baud rates constants")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconstant +tcssetattr-actions+ (list :tcsanow :tcsadrain :tcsaflush)
    "Valid constants for tcsetattr action parameter")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconstant +lflags+ (list :isig :icanon :xcase :echo :echoe :echok :echonl
			      :noflsh :iexten :echoctl :echoprt :echoke :flusho
			      :pendin :tostop)
    "Termios lflag filed constants")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconstant +iflags+ (list :inpck :ignpar :parmrk :istrip :ixon :ixoff
			      :ixany :ignbrk :brkint :inlcr :igncr :icrnl
			      :iuclc :imaxbel)
    "Termios iflag filed constants")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconstant +oflags+ (list :opost :olcuc :onlcr :ocrnl :nocr :onlret :ofill
			      :ofdel :nldly :nl0 :nl1 :crdly :cr0 :cr1 :cr2 :cr3
			      :tabdly :tab0 :tab1 :tab2 :tab3 :bsdly :bs0 :bs1
			      :vtdly :vt0 :vt1 :ffdly :ff0 :ff1)
    "Termios oflag filed constants")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconstant +control-characters+ (list :vintr :vquit :verase :vkill :veof
					  :veol :veol2 :vmin :vtime)
    "Termios control character constants for termios cc field")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )					; </ eval-when >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
