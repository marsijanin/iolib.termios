;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;; Termios (3p) api wrappers for iolib - streams abstraction

(in-package #:iolib.serial)

(defclass dual-channel-tty-gray-stream (dual-channel-gray-stream)
  ((path :reader tty-path :initarg :path :type string)
   (original-settings :reader original-settings :initarg :original-settings))
  (:documentation "Gray stream class for serial devices"))

(defun open-serial-stream (path &key (flag (logior isys:o-rdwr isys:o-nonblock isys:o-noctty))
                        (mode isys:*default-open-mode*)
			(external-format :default))
  "Return `dual-channel-tty-gray-stream' instances associated
   with serial device"
  (let ((fd (isys:open path flag mode))
        (termios (foreign-alloc 'termios)))
    (%tcgetattr fd termios)
    (let ((s (make-instance 'dual-channel-tty-gray-stream
                            :fd fd
                            :path path
                            :external-format external-format
                            :original-settings termios)))
      (trivial-garbage:finalize s (lambda () (foreign-free termios)))
      s)))

;; Close method for dual-channel-single-fd-gray-stream in iolib.streams,
;; (which call %sys-close) is :around too.
;; No need to call cancel-finalization case another close method will do it.
(defmethod close :around ((stream dual-channel-tty-gray-stream) &key abort)
  (declare (ignorable abort))
  (when (fd-of stream)
    (%tcsetattr (fd-of stream) :tcsanow (original-settings stream))
    (foreign-free (original-settings stream))
    (setf (slot-value stream 'original-settings) nil))
  (call-next-method))

(defmacro with-serial-stream ((stream path
                                      &key (speed :b115200)
                                      (parity :n)
                                      (byte-size 8)
                                      ;; not an posix option
                                      #+(or bsd linux)hardware-flow-control
                                      software-flow-control
                                      (flag (logior isys:o-rdwr
                                                    isys:o-nonblock
                                                    isys:o-noctty))
                                      (mode isys:*default-open-mode*)
                                      (external-format :default))
                              &body body)
  "Wrapper around `with-open-stream' with a few settings for serial device:
   - speed: baud rate. An integer (stty will look for a corresponding
     baud rate constant and use it, or signal an error otherwise),
     or constant symbol;
   - parity: one of `:n' (no parity checking), `:e' (even), `:o' (odd),
     or `:s' (space parity);
   - byte-size: character size as it described in
     `posix serial programming manual', can be an integer in 5-8 (inclusive)
     diapason;
   - `hardware/software-flow-control' for enable hardware/software flow control
     (disable by default). Note, what hardware flow control (crtscts) is not an
     posix feature, so it can be not implemented on some platforms.

     I.e. 9600-8n1 mode will be `:speed :b9600 :parity :n :byte-size 8`,
     and 300-5e1 `:speed :b300 :parity :e :byte-size 5`. 115200-8n1 is default.

   Other &key parameters is:
   - flag & mode: passed to `isys:open',
     (logior isys:o-rdwr isys:o-nonblock isys:o-noctty)
     & isys:*default-open-mode* by default.
   - external-format: see babel manual.
   - stream read/write timeout values: no default values are specified.


   Example (if rx of /dev/ttyUSB0 connected with rx should return T):
   (itty:with-serial-stream (tty \"/dev/ttyUSB0\"
                                 :speed :b38400)
	   (let ((str \"test\"))
	     (write-line str tty)
	     (finish-output tty)
	     (sleep .1)
	     (string= (read-line tty) str)))
  "
  `(with-open-stream
       (,stream (open-serial-stream ,path
                                    :flag ,flag
                                    :mode ,mode
                                    :external-format ,external-format))
     (stty ,stream
           ;; speed first
           ,speed
           ;; do not block on reading
           :raw t :vtime 0 :vmin 0
           ;; allways reset csize before set byte size
           :csize nil
           ;; parity
           ,@(case parity
                   (:n '(:parenb nil :cstopb nil))
                   (:e '(:parenb t :parodd nil :cstopb nil))
                   (:o '(:parenb t :parodd t :cstopb nil))
                   (:s '(:parenb nil :parodd nil :cstopb nil))
                   (t
                    (error "Unknown parity checking mode ~a" parity)))
           ;; byte size
           ,@(case byte-size
                   (5 '(:cs5 t))
                   (6 '(:cs6 t))
                   (7 '(:cs7 t))
                   (8 '(:cs8 t))
                   (t (error "Byte size ~a is unsupported" byte-size)))
           ;; hardware flow control
           #+(or bsd linux),@(when hardware-flow-control '(:crtscts t))
           ,@(when software-flow-control '(:ixon t :ixoff t)))
     ,@body))

(defmacro with-serial-streams (binds &body body)
  "Multiple `with-serial-streams` variant"
  (if binds
      `(with-serial-stream ,(car binds)
         (with-serial-streams ,(cdr binds)
           ,@body))
      `(progn ,@body)))
;;;; EOF

