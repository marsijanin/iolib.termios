;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;; Termios (3p) api wrappers for iolib - Grovel for termios constants.

;;;; TODO: check non POSXI features for other (non linux) systems: i.e
;;;; #+bsd etc.  Currently non POSIX features are #+linux marked or
;;;; #+(or linux bsd) marked if they requires _BSD_SOURCE
;;;; TODO: libfixposix migration?

(in-package #:iolib.serial)

(include "termios.h")

;; for some non posix features
(define "_XOPEN_SOURCE")                
(define "_BSD_SOURCE")                  

;;;
(constantenum (cflag :define-constants t)
  #+(or linux bsd)
  ((:cbaud "CBAUD"))
  #+(or linux bsd)
  ((:cbaudex "CBAUDEX"))
  ((:csize "CSIZE"))
  ;; valid character sizes
  ((:cs5 "CS5"))
  ((:cs6 "CS6"))
  ((:cs7 "CS7"))
  ((:cs8 "CS8"))
  ;; valid character sizes
  ((:cstopb "CSTOPB"))
  ((:cread "CREAD"))
  ((:parenb "PARENB"))
  ((:parodd "PARODD"))
  ((:hupcl "HUPCL"))
  ((:clocal "CLOCAL"))
  #-linux
  ((:loblk "LOBLK"))
  #+(or bsd linux)
  ((:cibaud "CIBAUD"))
  #+(or bsd linux)
  ((:cmspar "CMSPAR"))
  #+(or bsd linux)
  ((:crtscts "CRTSCTS")))

;;; lflags
(constantenum (lflag :define-constants t)
  ((:isig "ISIG"))
  ((:icanon "ICANON"))
  #-linux
  ((:xcase "XCASE"))
  ((:echo "ECHO"))
  ((:echoe "ECHOE"))
  ((:echok "ECHOK"))
  ((:echonl "ECHONL"))
  #+(or linux bsd)
  ((:echoctl "ECHOCTL"))
  #+(or linux bsd)
  ((:echoprt "ECHOPRT"))
  #+(or linux bsd)
  ((:echoke "ECHOKE"))
  #-linux
  ((:defecho "DEFECHO"))
  #+bsd
  ((:flusho "FLUSHO"))
  ((:noflsh "NOFLSH"))
  ((:tostop "TOSTOP"))
  #+bsd
  ((:pendin "PENDIN"))
  ((:iexten "IEXTEN")))

;;; iflags
(constantenum (iflag :define-constants t)
  ((:ignbrk "IGNBRK"))
  ((:brkint "BRKINT"))
  ((:ignpar "IGNPAR"))
  ((:parmrk "PARMRK"))
  ((:inpck "INPCK"))
  ((:istrip "ISTRIP"))
  ((:inlcr "INLCR"))
  ((:igncr "IGNCR"))
  ((:icrnl "ICRNL"))
  #+linux
  ((:iuclc "IUCLC"))
  ((:ixon "IXON"))
  ;; XSI features are #+xfi marked in sb-posix grovel file,
  ;; but (find :xsi *features*) return NIL
  ;; so i'm leaving xsi features unmarked 
  ((:ixany "IXANY"))
  ((:ixoff "IXOFF"))
  #-linux
  ((:imaxbel "IMAXBEL"))
  #+linux
  ((:iutf8 "IUTF8")))

;;; oflags
(constantenum (oflag :define-constants t)
  ((:opost "OPOST"))
  #+linux
  ((:olcuc "OLCUC"))
  ((:onlcr "ONLCR"))
  ((:ocrnl "OCRNL"))
  ((:onocr "ONOCR"))
  ((:onlret "ONLRET"))
  ((:ofill "OFILL"))
  #-linux
  ((:ofdel "OFDEL"))
  #+(or linux bsd)
  ((:nldly "NLDLY"))
  #+(or linux bsd)
  ((:crdly "CRDLY"))
  #+(or linux bsd)
  ((:tabdly "TABDLY"))
  #+(or linux bsd)
  ((:bsdly "BSDLY"))
  #+(or linux bsd)
  ((:vtdly "VTDLY"))
  #+(or linux bsd)
  ((:ffdly "FFDLY")))

;;; control characters
(constantenum (control-character :define-constants t)
  ((:vintr "VINTR"))
  ((:vquit "VQUIT"))
  ((:verase "VERASE"))
  ((:vkill "VKILL"))
  ((:veof "VEOF"))
  ((:vmin "VMIN"))
  ((:veol "VEOL"))
  ((:vtime "VTIME"))
  #+linux
  ((:veol2 "VEOL2"))
  #-linux
  ((:vswtch "VSWTCH"))
  ((:vstart "VSTART"))
  ((:vstop "VSTOP"))
  ((:vsusp "VSUSP"))
  #-linux
  ((:vdsusp "VDSUSP"))
  #+linux
  ((:vlnext "VLNEXT"))
  #+linux
  ((:vwerase "VWERASE"))
  #+linux
  ((:vreprint "VREPRINT"))
  #-linux
  ((:vdiscard "VDISCARD"))
  #-linux
  ((:vstatus "VSTATUS")))

;;; tcssetattr actions
(constantenum (tcssetattr-action :define-constants t)
  ((:tcsanow "TCSANOW"))
  ((:tcsadrain "TCSADRAIN"))
  ((:tcsaflush "TCSAFLUSH")))

;;; baud rates
(constantenum (baud-rate :define-constants t)
  ;; Posix baud rates:
  ((:b0       "B0"))
  ((:b50      "B50"))
  ((:b75      "B75"))
  ((:b110     "B110"))
  ((:b134     "B134"))
  ((:b150     "B150"))
  ((:b200     "B200"))
  ((:b300     "B300"))
  ((:b600     "B600"))
  ((:b1200    "B1200"))
  ((:b1800    "B1800"))
  ((:b2400    "B2400"))
  ((:b4800    "B4800"))
  ((:b9600    "B9600"))
  ((:b19200   "B19200"))
  ((:b38400   "B38400"))
  ((:b57600   "B57600"))
  ((:b115200  "B115200"))
  ;; Non posix baud rates:
  ((:b230400  "B230400"))
  ((:b460800  "B460800"))
  ((:b500000  "B500000"))
  ((:b576000  "B576000"))
  ((:b921600  "B921600"))
  ((:b1000000 "B1000000"))
  ((:b1152000 "B1152000"))
  ((:b1500000 "B1500000"))
  ((:b2000000 "B2000000"))
  ((:b2500000 "B2500000"))
  ((:b3000000 "B3000000"))
  ((:b3500000 "B3500000"))
  ((:b4000000 "B4000000")))
;;;; EOF
