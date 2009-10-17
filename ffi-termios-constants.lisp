;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; Grovel for termios constants.
;; TODO: check non POSXI features for other (non linux) systems: i.e #+bsd etc.
;; Currently non POSIX features are #+linux marked or #(or linux bsd) marked
;; if they requires _BSD_SOURCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.termios)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(include "termios.h")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for some non posix features
(define "_XOPEN_SOURCE")                
(define "_BSD_SOURCE")                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < clfags >
#+(or linux bsd)
(constant (cbaud "CBAUD")
          :documentation
          " Baud speed mask (4+1 bits).")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or linux bsd)
(constant (cbaudex "CBAUDEX")
          :documentation
          "Extra baud speed mask (1 bit),included in CBAUD. ")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (csize "CSIZE")
          :documentation
          "Character size mask.  Values are CS5, CS6, CS7, or CS8.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < valid character sizes >
(constant (cs5 "CS5"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (cs6 "CS6"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (cs7 "CS7"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (cs8 "CS8"))
;; </ valid character sizes >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (cstopb "CSTOPB")
          :documentation
          "Set two stop bits, rather than one.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (cread "CREAD")
          :documentation "Enable receiver. This flag is really necessary.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (parenb "PARENB")
          :documentation
          "Enable parity generation on output and parity checking for input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (parodd "PARODD")
          :documentation
          "If  set,  then parity  for  input  and  output is  odd;
	   otherwise even parity is used.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (hupcl "HUPCL")
          :documentation
          "Lower modem control lines after last process closes the
	   device (hang up).")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (clocal "CLOCAL") :documentation "Ignore modem control lines.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-linux
(constant (loblk "LOBLK")
          :documentation
          "Block  output from a  non-current shell layer.
           For use by shl (shell layers).")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or bsd linux)
(constant (cibaud "CIBAUD")
          :documentation
          "Mask for input speeds. The values for the CIBAUD bits are the same
           as the values for the CBAUD bits,shifted left IBSHIFT bits. ")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or bsd linux)
(constant (cmspar "CMSPAR")
          :documentation
          "Use `stick' (mark/space) parity
           (supported on certain serial devices):
           if  PARODD is set, the parity bit is  always 1;
           if PARODD is not set,then the parity bit is always 0).")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or bsd linux)
(constant (crtscts "CRTSCTS")
          :documentation "Enable RTS/CTS (hardware) flow control. ")
;; </ cflags >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < lflags >
(constant (isig "ISIG")
          :documentation
          "When any of the characters INTR, QUIT, SUSP, or DSUSP
           are received, generate the corresponding signal.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (icanon "ICANON") :documentation "Enable canonical mode.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-linux
(constant (xcase "XCASE")
          :documentation
          "If ICANON is also  set, terminal is uppercase only.
           Input is converted to lowercase, except for characters preceded
           by \.  On output,  uppercase characters are preceded by
	   \ and lowercase  characters are converted to uppercase.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (echo "ECHO") :documentation "Echo input characters.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (echoe "ECHOE")
          :documentation
          "If ICANON is also set, the ERASE character erases the
	   preceding input character, and WERASE erases the
	   preceding word.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (echok "ECHOK")
          :documentation
          "If ICANON is also set, the KILL character erases the current line.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (echonl "ECHONL")
          :documentation
          "If ICANON is also set, echo the NL character
           even if ECHO is not set.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or linux bsd)
(constant (echoctl "ECHOCTL")
          :documentation
          "If ECHO is  also  set, ASCII control signals other than TAB,
           NL, START, and STOP are echoed as ^X, where X is the character
           with ASCII code 0x40 greater than the control signal.
           For  example, character  0x08  (BS) is echoed as  ^H.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or linux bsd)
(constant (echoprt "ECHOPRT")
          :documentation
          "If ICANON and IECHO are also set, characters are printed
           as they are being erased.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or linux bsd)
(constant (echoke "ECHOKE")
          :documentation
          "If ICANON is also set, KILL is echoed by erasing each character
           on the line, as  specified by ECHOE and ECHOPRT.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-linux
(constant (defecho "DEFECHO")
          :documentation " Echo only when a process is reading.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+bsd
(constant (flusho "FLUSHO")
          :documentation
          "Output is being flushed.
           This flag is toggled by typing the DISCARD character.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (noflsh "NOFLSH")
          :documentation
          "Disable  flushing  the  input  and output  queues  when
           generating the SIGINT, SIGQUIT, and SIGSUSP signals.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (tostop "TOSTOP")
          :documentation
          "Send  the SIGTTOU signal to the process group of
           a background process which tries to write to
           its controlling terminal.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+bsd
(constant (pendin "PENDIN")
          :documentation
          "All characters in the input queue are reprinted when the
           next character is read. (bash(1) handles typeahead this way.)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (iexten "IEXTEN")
          :documentation
          "Enable implementation-defined input processing.
           This flag, as well as ICANON must be enabled for the special
           characters EOL2, LNEXT, REPRINT, WERASE to be interpreted,
           and for the IUCLC flag to be effective.")
;; </ lflags >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < iflags >
(constant (ignbrk "IGNBRK") :documentation "Ignore BREAK condition on input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (brkint "BRKINT")
          :documentation
          "If IGNBRK is set, a BREAK is ignored.  If it is not set
           but BRKINT  is set, then  a BREAK causes the  input and
	   output queues to be flushed, and if the terminal is the
	   controlling terminal of  a foreground process group, it
	   will  cause a  SIGINT  to be  sent  to this  foreground
	   process group.  When neither IGNBRK nor BRKINT are set,
	   a BREAK reads as a null byte ('0'), except when PARMRK
	   is set, in which case  it reads as the sequence \377 \0 \0.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (ignpar "IGNPAR")
          :documentation "Ignore framing errors and parity errors.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (parmrk "PARMRK")
          :documentation
          "If IGNPAR is not set, prefix a character with a parity
           error or framing error with 377 0. If neither IGNPAR
	   nor PARMRK is set, read a character with a parity error
	   or framing error as 0.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (inpck "INPCK") :documentation "Enable input parity checking.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (istrip "ISTRIP") :documentation "Strip off eighth bit.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (inlcr "INLCR") :documentation "Translate NL to CR on input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (igncr "IGNCR") :documentation "Ignore carriage return on input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (icrnl "ICRNL")
          :documentation
          "Translate carriage return to newline on input
           (unless IGNCR is set).")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+linux
(constant (iuclc "IUCLC")
          :documentation "Map uppercase characters to lowercase on input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (ixon "IXON")
          :documentation "Enable XON/XOFF flow control on output.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XSI features are #+xfi marked in sb-posix grovel file,
;; but (find :xsi *features*) return NIL
;; so i'm leaving xsi features unmarked 
(constant (ixany "IXANY")
          :documentation
          "Typing any character will restart stopped output.
           (The default is to allow just the START character
           to restart output.)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (ixoff "IXOFF")
          :documentation "Enable XON/XOFF flow control on input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-linux
(constant (imaxbel "IMAXBEL")
          :documentation "Ring bell when input queue is full.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+linux
(constant (iutf8 "IUTF8")
          :documentation
          "Input is UTF8; this allows character-erase to be correctly performed
           in cooked mode.")
;; </ iflags >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < oflags >
(constant (opost "OPOST")
          :documentation "Enable implementation-defined output processing.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+linux
(constant (olcuc "OLCUC")
          :documentation "Map lowercase characters to uppercase on output.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (onlcr "ONLCR") :documentation "Map NL to CR-NL on output.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (ocrnl "OCRNL") :documentation "Map CR to NL on output.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (onocr "ONOCR") :documentation "Don't output CR at column 0.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (onlret "ONLRET") :documentation "Don't output CR.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (ofill "OFILL")
          :documentation
          "Send fill characters for a delay, rather than using a timed delay.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-linux
(constant (ofdel "OFDEL")
          :documentation
          "Fill character is ASCII DEL (0177). If unset, fill character
           is ASCII NUL ('0').")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or linux bsd)
(constant (nldly "NLDLY")
          :documentation
          "Newline delay mask. Values are NL0 and NL1.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or linux bsd)
(constant (crdly "CRDLY")
          :documentation
          "Carriage return delay mask. Values are CR0, CR1, CR2,or  CR3.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or linux bsd)
(constant (tabdly "TABDLY")
          :documentation
          "Horizontal tab delay mask.
           Values are TAB0, TAB1,TAB2, TAB3 (or XTABS).
           A value  of  TAB3, that is, XTABS, expands tabs to spaces
           (with tab  stops every eight columns).")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or linux bsd)
(constant (bsdly "BSDLY")
          :documentation "Backspace  delay mask. Values are BS0 or BS1.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or linux bsd)
(constant (vtdly "VTDLY")
          :documentation "Vertical tab delay mask.  Values are VT0 or VT1.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or linux bsd)
(constant (ffdly "FFDLY")
          :documentation "Form  feed delay  mask. Values  are   FF0  or  FF1.")
;; </ oflags >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < control-characters >
(constant (vintr "VINTR")
          :documentation
          "(003, ETX, Ctrl-C, or also 0177, DEL, rubout) Interrupt character.
           Send a SIGINT signal. Recognized when ISIG is set,
           and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (vquit "VQUIT")
          :documentation
          "(034, FS, Ctrl-) Quit character.  Send SIGQUIT signal.
           Recognized  when ISIG is  set, and  then not  passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (verase "VERASE")
          :documentation
          "(0177, DEL, rubout,  or 010,  BS, Ctrl-H, or also #)
           Erase    character.    This    erases    the   previous
           not-yet-erased character,  but does not  erase past EOF
	   or beginning-of-line.   Recognized when ICANON  is set,
	   and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (vkill "VKILL")
          :documentation
          "(025, NAK, Ctrl-U, or Ctrl-X, or also @) Kill character.
           This erases the input since the last EOF or beginning-of-line.
           Recognized when ICANON is set, and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (veof "VEOF")
          :documentation
          "(004, EOT, Ctrl-D) End-of-file character.
           More precisely: this character causes the pending tty buffer
           to be sent to the waiting user program without waiting for
           end-of-line.  If it is the first character of the line,
           the read(2) in the user program returns 0, which signifies
           end-of-file.
           Recognized  when ICANON is set, and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (vmin "VMIN")
          :documentation
          "Minimum number of characters for non-canonical read.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (veol "VEOL")
          :documentation
          "(0, NUL) Additional end-of-line character.
           Recognized when ICANON is set.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (vtime "VTIME")
          :documentation "Timeout in deciseconds for non-canonical read.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+linux
(constant (veol2 "VEOL2")
          :documentation
          "Yet another end-of-line character.
           Recognized when ICANON is set.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-linux
(constant (vswtch "VSWTCH")
          :documentation "Switch character.  (Used by shl only.)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (vstart "VSTART")
          :documentation
          "(021, DC1, Ctrl-Q) Start character.
            Restarts output stopped by the Stop character.
            Recognized when IXON is set, and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (vstop "VSTOP")
          :documentation
          "(023, DC3, Ctrl-S) Stop character.
           Stop output until Start character typed.
           Recognized  when IXON  is set, and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (vsusp "VSUSP")
          :documentation
          "(032, SUB, Ctrl-Z) Suspend character.
           Send SIGTSTP signal. Recognized when ISIG is set,
           and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-linux
(constant (vdsusp "VDSUSP")
          :documentation
          "031, EM, Ctrl-Y) Delayed suspend  character: send SIGTSTP signal
           when  the  character  is  read  by  the  user  program.
           Recognized when IEXTEN and ISIG are set, and the system
	   supports job control, and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+linux
(constant (vlnext "VLNEXT")
          :documentation
          "(026, SYN, Ctrl-V) Literal next.
           Quotes the next  input character,  depriving it of  a possible
           special  meaning.  Recognized when  IEXTEN is  set,
           and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+linux
(constant (vwerase "VWERASE")
          :documentation
          "(027, ETB, Ctrl-W) Word  erase.
           Recognized when ICANON and IEXTEN are set,
           and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+linux
(constant (vreprint "VREPRINT")
          :documentation
          "(022, DC2, Ctrl-R) Reprint unread characters.
           Recognized when ICANON and IEXTEN are set,
           and then not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-linux
(constant (vdiscard "VDISCARD")
          :documentation
          "017, SI, Ctrl-O) Toggle: start/stop discarding  pending output.
           Recognized when  IEXTEN is set, and then  not passed as input.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-linux
(constant (vstatus "VSTATUS")
          :documentation "Status request: 024, DC4, Ctrl-T).")
;; </ control characters >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < tcssetattr actions >
(constant (tcsanow "TCSANOW")
          :documentation "The change (by tcssetattr) occurs immediately.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (tcsadrain "TCSADRAIN")
          :documentation
          "The change occurs (by tcssetattr) after all output written to fd
           has been  transmitted.  This function  should be used when
	   changing parameters that affect output.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (tcsaflush "TCSAFLUSH")
          :documentation
          "The change (by tcssetattr) occurs after all output written to the
           object referred by fd has been transmitted, and all
	   input that has been received but not read will be
           discarded before the change is made.")
;; </ tcssetattr actions >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < Posix baud rates >
(constant (b0 "B0"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b50 "B50"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b75 "B75"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b110 "B110"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b134 "B134"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b150 "B150"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b200 "B200"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b300 "B300"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b600 "B600"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b1200 "B1200"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b1800 "B1800"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b2400 "B2400"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b4800 "B4800"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b9600 "B9600"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b19200 "B19200"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b38400 "B38400"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b57600 "B57600"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b115200 "B115200"))
;; </ Posix baud rates >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < Non posix baud rates >
(constant (b230400 "B230400"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b460800 "B460800"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b500000 "B500000"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b576000 "B576000"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b921600 "B921600"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b1000000 "B1000000"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b1152000 "B1152000"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b1500000 "B1500000"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b2000000 "B2000000"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b2500000 "B2500000"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b3000000 "B3000000"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b3500000 "B3500000"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(constant (b4000000 "B4000000"))
;; </ Non posix baud rates >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
