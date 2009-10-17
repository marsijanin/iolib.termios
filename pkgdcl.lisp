;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; termios (3p) api wrapers -  package definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :iolib.termios
  (:nicknames #:itty)
  (:use :iolib.base :iolib.syscalls :iolib.streams :cffi)
  (:export
   ;; Struct termios:
   #:termios
   ;; cflags:
   #:cbaud
   #:exta
   #:extb
   #:csize
   #:cs5
   #:cs6
   #:cs7
   #:cs8
   #:cstopb
   #:cread
   #:parenb
   #:parodd
   #:hupcl
   #:clocal
   #:loblk
   #:crtscts
   ;; baud rates:
   #:b0
   #:b50
   #:b75
   #:b110
   #:b134
   #:b150
   #:b200
   #:b300
   #:b600
   #:b1200
   #:b1800
   #:b2400
   #:b4800
   #:b9600
   #:b19200
   #:b38400
   #:b57600
   #:b76800
   #:b115200
   ;; tcsetattr actions:
   #:tcsanow
   #:tcsadrain
   #:tcsaflush
   ;; lflags:
   #:isig
   #:icanon
   #:xcase
   #:echo
   #:echoe
   #:echok
   #:echonl
   #:noflsh
   #:iexten
   #:echoctl
   #:echoprt
   #:echoke
   #:flusho
   #:pendin
   #:tostop
   ;; iflags:
   #:inpck
   #:ignpar
   #:parmrk
   #:istrip
   #:ixon
   #:ixoff
   #:ixany
   #:ignbrk
   #:brkint
   #:inlcr
   #:igncr
   #:icrnl
   #:iuclc
   #:imaxbel
   ;; oflags:
   #:opost
   #:olcuc
   #:onlcr
   #:ocrnl
   #:nocr
   #:onlret
   #:ofill
   #:ofdel
   #:nldly
   #:nl0
   #:nl1
   #:crdly
   #:cr0
   #:cr1
   #:cr2
   #:cr3
   #:tabdly
   #:tab0
   #:tab1
   #:tab2
   #:tab3
   #:bsdly
   #:bs0
   #:bs1
   #:vtdly
   #:vt0
   #:vt1
   #:ffdly
   #:ff0
   #:ff1
   ;; control characters:
   #:vintr
   #:vquit
   #:verase
   #:vkill
   #:veof
   #:veol
   #:veol2
   #:vmin
   #:vtime
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
   ;; useful wrapers:
   #:stty
   ;; additional stty options:
   #:raw
   #:cooked
   #:oddp
   #:parity
   #:really-raw
   #:open-tty-stream
   #:with-tty-stream
   #:with-raw-serial))			; </ defpackage >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
