;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; Grovel for termios constants.
;; TODO: check non POSXI features for other (non linux) systems: i.e #+bsd etc.
;; Currently non POSIX features are #+linux marked or #+(or linux bsd) marked
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
(constantenum (cflag :define-constants t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:cbaud "CBAUD")
   :documentation "Baud speed mask (4+1 bits).")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:cbaudex "CBAUDEX")
   :documentation "Extra baud speed mask (1 bit),included in CBAUD. ")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:csize "CSIZE")
   :documentation "Character size mask.  Values are CS5, CS6, CS7, or CS8.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; < valid character sizes >
  ((:cs5 "CS5"))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:cs6 "CS6"))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:cs7 "CS7"))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:cs8 "CS8"))
  ;; </ valid character sizes >
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:cstopb "CSTOPB")
   :documentation "Set two stop bits, rather than one.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:cread "CREAD")
   :documentation "Enable receiver. This flag is really necessary.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:parenb "PARENB")
   :documentation
   "Enable parity generation on output and parity checking for input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:parodd "PARODD")
   :documentation
   "If  set,  then parity  for  input  and  output is  odd;
	   otherwise even parity is used.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:hupcl "HUPCL")
   :documentation
   "Lower modem control lines after last process closes the
	   device (hang up).")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:clocal "CLOCAL") :documentation "Ignore modem control lines.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #-linux
  ((:loblk "LOBLK")
   :documentation
   "Block  output from a  non-current shell layer.
    For use by shl (shell layers).")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or bsd linux)
  ((:cibaud "CIBAUD")
   :documentation
   "Mask for input speeds. The values for the CIBAUD bits are the same
    as the values for the CBAUD bits,shifted left IBSHIFT bits. ")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or bsd linux)
  ((:cmspar "CMSPAR")
   :documentation
   "Use `stick' (mark/space) parity
    (supported on certain serial devices):
    if  PARODD is set, the parity bit is  always 1;
    if PARODD is not set,then the parity bit is always 0).")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or bsd linux)
  ((:crtscts "CRTSCTS")
   :documentation "Enable RTS/CTS (hardware) flow control.")) ; </cflags>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <lflags>
(constantenum (lflag :define-constants t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:isig "ISIG")
   :documentation
   "When any of the characters INTR, QUIT, SUSP, or DSUSP
    are received, generate the corresponding signal.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:icanon "ICANON") :documentation "Enable canonical mode.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #-linux
  ((:xcase "XCASE")
   :documentation
   "If ICANON is also  set, terminal is uppercase only.
    Input is converted to lowercase, except for characters preceded
    by \.  On output,  uppercase characters are preceded by
    \\ and lowercase  characters are converted to uppercase.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:echo "ECHO") :documentation "Echo input characters.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:echoe "ECHOE")
   :documentation
   "If ICANON is also set, the ERASE character erases the
	   preceding input character, and WERASE erases the
	   preceding word.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:echok "ECHOK")
   :documentation
   "If ICANON is also set, the KILL character erases the current line.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:echonl "ECHONL")
   :documentation
   "If ICANON is also set, echo the NL character
           even if ECHO is not set.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:echoctl "ECHOCTL")
   :documentation
   "If ECHO is  also  set, ASCII control signals other than TAB,
    NL, START, and STOP are echoed as ^X, where X is the character
    with ASCII code 0x40 greater than the control signal.
    For  example, character  0x08  (BS) is echoed as  ^H.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:echoprt "ECHOPRT")
   :documentation
   "If ICANON and IECHO are also set, characters are printed
    as they are being erased.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:echoke "ECHOKE")
   :documentation
   "If ICANON is also set, KILL is echoed by erasing each character
    on the line, as  specified by ECHOE and ECHOPRT.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #-linux
  ((:defecho "DEFECHO")
   :documentation " Echo only when a process is reading.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+bsd
  ((:flusho "FLUSHO")
   :documentation
   "Output is being flushed.
    This flag is toggled by typing the DISCARD character.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:noflsh "NOFLSH")
   :documentation
   "Disable  flushing  the  input  and output  queues  when
           generating the SIGINT, SIGQUIT, and SIGSUSP signals.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:tostop "TOSTOP")
   :documentation
   "Send  the SIGTTOU signal to the process group of
           a background process which tries to write to
           its controlling terminal.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+bsd
  ((:pendin "PENDIN")
   :documentation
   "All characters in the input queue are reprinted when the
    next character is read. (bash(1) handles typeahead this way.)")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:iexten "IEXTEN")
   :documentation
   "Enable implementation-defined input processing.
    This flag, as well as ICANON must be enabled for the special
    characters EOL2, LNEXT, REPRINT, WERASE to be interpreted,
    and for the IUCLC flag to be effective.")) ; </lflags>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <iflags>
(constantenum (iflag :define-constants t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:ignbrk "IGNBRK") :documentation "Ignore BREAK condition on input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:brkint "BRKINT")
   :documentation
   "If IGNBRK is set, a BREAK is ignored.  If it is not set
    but BRKINT  is set, then  a BREAK causes the  input and
    output queues to be flushed, and if the terminal is the
    controlling terminal of  a foreground process group, it
    will  cause a  SIGINT  to be  sent  to this  foreground
    process group.  When neither IGNBRK nor BRKINT are set,
    a BREAK reads as a null byte ('0'), except when PARMRK
    is set, in which case  it reads as the sequence \377 \0 \0.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:ignpar "IGNPAR")
   :documentation "Ignore framing errors and parity errors.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:parmrk "PARMRK")
   :documentation
   "If IGNPAR is not set, prefix a character with a parity
    error or framing error with 377 0. If neither IGNPAR
    nor PARMRK is set, read a character with a parity error
    or framing error as 0.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:inpck "INPCK") :documentation "Enable input parity checking.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:istrip "ISTRIP") :documentation "Strip off eighth bit.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:inlcr "INLCR") :documentation "Translate NL to CR on input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:igncr "IGNCR") :documentation "Ignore carriage return on input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:icrnl "ICRNL")
   :documentation
   "Translate carriage return to newline on input
    (unless IGNCR is set).")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+linux
  ((:iuclc "IUCLC")
   :documentation "Map uppercase characters to lowercase on input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:ixon "IXON")
   :documentation "Enable XON/XOFF flow control on output.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; XSI features are #+xfi marked in sb-posix grovel file,
  ;; but (find :xsi *features*) return NIL
  ;; so i'm leaving xsi features unmarked 
  ((:ixany "IXANY")
   :documentation
   "Typing any character will restart stopped output.
           (The default is to allow just the START character
           to restart output.)")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:ixoff "IXOFF")
   :documentation "Enable XON/XOFF flow control on input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #-linux
  ((:imaxbel "IMAXBEL")
   :documentation "Ring bell when input queue is full.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+linux
  ((:iutf8 "IUTF8")
   :documentation
   "Input is UTF8; this allows character-erase to be correctly performed
    in cooked mode."))                  ; </iflags>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <oflags>
(constantenum (oflag :define-constants t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:opost "OPOST")
   :documentation "Enable implementation-defined output processing.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+linux
  ((:olcuc "OLCUC")
   :documentation "Map lowercase characters to uppercase on output.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:onlcr "ONLCR") :documentation "Map NL to CR-NL on output.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:ocrnl "OCRNL") :documentation "Map CR to NL on output.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:onocr "ONOCR") :documentation "Don't output CR at column 0.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:onlret "ONLRET") :documentation "Don't output CR.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:ofill "OFILL")
   :documentation
   "Send fill characters for a delay, rather than using a timed delay.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #-linux
  ((:ofdel "OFDEL")
   :documentation
   "Fill character is ASCII DEL (0177). If unset, fill character
    is ASCII NUL ('0').")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:nldly "NLDLY")
   :documentation
   "Newline delay mask. Values are NL0 and NL1.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:crdly "CRDLY")
   :documentation
   "Carriage return delay mask. Values are CR0, CR1, CR2,or  CR3.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:tabdly "TABDLY")
   :documentation
   "Horizontal tab delay mask.
    Values are TAB0, TAB1,TAB2, TAB3 (or XTABS).
    A value  of  TAB3, that is, XTABS, expands tabs to spaces
    (with tab  stops every eight columns).")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:bsdly "BSDLY")
   :documentation "Backspace  delay mask. Values are BS0 or BS1.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:vtdly "VTDLY")
   :documentation "Vertical tab delay mask.  Values are VT0 or VT1.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+(or linux bsd)
  ((:ffdly "FFDLY")
   :documentation "Form feed delay mask. Values are FF0 or FF1.")) ;</oflags>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <control characters>
(constantenum (control-character :define-constants t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:vintr "VINTR")
   :documentation
   "(003, ETX, Ctrl-C, or also 0177, DEL, rubout) Interrupt character.
    Send a SIGINT signal. Recognized when ISIG is set,
    and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:vquit "VQUIT")
   :documentation
   "(034, FS, Ctrl-) Quit character.  Send SIGQUIT signal.
    Recognized  when ISIG is  set, and  then not  passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:verase "VERASE")
   :documentation
   "(0177, DEL, rubout,  or 010,  BS, Ctrl-H, or also #)
           Erase    character.    This    erases    the   previous
           not-yet-erased character,  but does not  erase past EOF
	   or beginning-of-line.   Recognized when ICANON  is set,
	   and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:vkill "VKILL")
   :documentation
   "(025, NAK, Ctrl-U, or Ctrl-X, or also @) Kill character.
    This erases the input since the last EOF or beginning-of-line.
    Recognized when ICANON is set, and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:veof "VEOF")
   :documentation
   "(004, EOT, Ctrl-D) End-of-file character.
    More precisely: this character causes the pending tty buffer
    to be sent to the waiting user program without waiting for
    end-of-line.  If it is the first character of the line,
    the read(2) in the user program returns 0, which signifies
    end-of-file.
    Recognized  when ICANON is set, and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:vmin "VMIN")
   :documentation
   "Minimum number of characters for non-canonical read.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:veol "VEOL")
   :documentation
   "(0, NUL) Additional end-of-line character.
           Recognized when ICANON is set.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:vtime "VTIME")
   :documentation "Timeout in deciseconds for non-canonical read.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+linux
  ((:veol2 "VEOL2")
   :documentation
   "Yet another end-of-line character.
    Recognized when ICANON is set.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #-linux
  ((:vswtch "VSWTCH")
   :documentation "Switch character.  (Used by shl only.)")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:vstart "VSTART")
   :documentation
   "(021, DC1, Ctrl-Q) Start character.
    Restarts output stopped by the Stop character.
    Recognized when IXON is set, and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:vstop "VSTOP")
   :documentation
   "(023, DC3, Ctrl-S) Stop character.
    Stop output until Start character typed.
    Recognized  when IXON  is set, and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:vsusp "VSUSP")
   :documentation
   "(032, SUB, Ctrl-Z) Suspend character.
           Send SIGTSTP signal. Recognized when ISIG is set,
           and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #-linux
  ((:vdsusp "VDSUSP")
   :documentation
   "031, EM, Ctrl-Y) Delayed suspend  character: send SIGTSTP signal
    when  the  character  is  read  by  the  user  program.
    Recognized when IEXTEN and ISIG are set, and the system
    supports job control, and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+linux
  ((:vlnext "VLNEXT")
   :documentation
   "(026, SYN, Ctrl-V) Literal next.
    Quotes the next  input character,  depriving it of  a possible
    special  meaning.  Recognized when  IEXTEN is  set,
   and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+linux
  ((:vwerase "VWERASE")
   :documentation
   "(027, ETB, Ctrl-W) Word  erase.
    Recognized when ICANON and IEXTEN are set,
    and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #+linux
  ((:vreprint "VREPRINT")
   :documentation
   "(022, DC2, Ctrl-R) Reprint unread characters.
    Recognized when ICANON and IEXTEN are set,
    and then not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #-linux
  ((:vdiscard "VDISCARD")
   :documentation
   "017, SI, Ctrl-O) Toggle: start/stop discarding  pending output.
    Recognized when  IEXTEN is set, and then  not passed as input.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #-linux
  ((:vstatus "VSTATUS")
   :documentation "Status request: 024, DC4, Ctrl-T).")) ;</control characters>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; < tcssetattr actions >
(constantenum (tcssetattr-action :define-constants t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:tcsanow "TCSANOW")
   :documentation "The change (by tcssetattr) occurs immediately.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:tcsadrain "TCSADRAIN")
   :documentation
   "The change occurs (by tcssetattr) after all output written to fd
    has been  transmitted.  This function  should be used when
    changing parameters that affect output.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ((:tcsaflush "TCSAFLUSH")
   :documentation
   "The change (by tcssetattr) occurs after all output written to the
    object referred by fd has been transmitted, and all
    input that has been received but not read will be
    discarded before the change is made.")) ; </tcssetattr actions>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <baud rates >
(constantenum (baud-rate :define-constants t)
  ;; <Posix baud rates>
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
  ;; </Posix baud rates>
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <Non posix baud rates>
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
  ((:b4000000 "B4000000")))             ;</Non posix baud rates> </baud rates>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
