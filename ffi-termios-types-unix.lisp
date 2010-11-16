;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;; Termios (3p) api wrappers for iolib - groveling for struct termios

(include "termios.h")

(in-package #:iolib.serial)

(ctype tcflag "tcflag_t")

(ctype cc "cc_t")

(ctype termios-speed "speed_t")

(constant (nccs "NCCS"))

(cstruct termios "struct termios" 
	 (iflag         "c_iflag" :type tcflag)
	 (oflag         "c_oflag" :type tcflag)
	 (cflag         "c_cflag" :type tcflag)
	 (lflag         "c_lflag" :type tcflag)
	 (control-chars "c_cc"    :type cc :count nccs))
;;;; EOF
