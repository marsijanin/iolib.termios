;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; streams for devices supported termios (3p) api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.termios)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass dual-channel-tty-gray-stream (dual-channel-single-fd-gray-stream)
  ((path :reader tty-path :initarg :path :type string)
   (read-timeout :accessor read-timeout :initarg :read-timeout)
   (write-timeout :accessor write-timeout :initarg :write-timeout)
   (original-settings :reader original-settings :initarg :original-settings))
  (:documentation "Gray stream class for serial devices"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unix signals such SIGTERM etc. are not usual conditions so with-open-stream
;; macro can't call close in order to restore original settings.
;; On the other hand installing signal handler is not a part
;; of the common-lip standard and not a purpose of this library.
;; So i guess that provide a list of open serial devices streams
;; for library users will be better solution.
(defparameter *open-serial-streams* nil
  "List of all open serial streams for restoring original serial devices
   settings via signal handlers.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun open-serial-stream (path &key (flag (logior o-rdwr o-nonblock o-noctty))
                        (mode *default-open-mode*)
			(external-format :default))
  "Return `dual-channel-tty-gray-stream' instances associated
   with serial device and push in into the `*open-serial-streams*' list"
  (let ((fd (%sys-open path flag mode))
        (termios (foreign-alloc 'termios)))
    (%tcgetattr fd termios)
    (let ((s (make-instance 'dual-channel-tty-gray-stream
                            :input-fd fd
                            :output-fd fd
                            :path path
                            :external-format external-format
                            :original-settings termios)))
      (push s *open-serial-streams*)
      s)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Close method for dual-channel-single-fd-gray-stream in iolib.streams,
;; (which call %sys-close) is :around too. 
(defmethod close :around ((stream dual-channel-tty-gray-stream) &key abort)
  (declare (ignorable abort))
  (when (fd-of stream)
    (%tcsetattr (fd-of stream) :tcsanow (original-settings stream))
    (foreign-free (original-settings stream))
    (setf (slot-value stream 'original-settings) nil)
    (removef *open-serial-streams* stream))
  (call-next-method))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod stream-read-sequence :before ((stream dual-channel-tty-gray-stream)
                                         sequence start end &key)
  (when (slot-boundp stream 'read-timeout)
    (iomux:wait-until-fd-ready (fd-of stream) :input (read-timeout stream) t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod stream-write-sequence :before ((stream dual-channel-tty-gray-stream)
                                         sequence start end &key)
  (when (slot-boundp stream 'write-timeout)
    (iomux:wait-until-fd-ready (fd-of stream) :output
                               (write-timeout stream) t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod stream-write-sequence :after ((stream dual-channel-tty-gray-stream)
                                         sequence start end
                                         &key (finish-output t))
  (when finish-output
    (stream-finish-output stream)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-serial-stream ((stream path
                                      &key (speed :b115200)
                                      (parity :n)
                                      (byte-size 8)
                                      ;; not an posix option
                                      #+(or bsd linux)hardware-flow-control
                                      software-flow-control
                                      (flag (logior o-rdwr o-nonblock o-noctty))
                                      (mode *default-open-mode*)
                                      (external-format :default)
                                      timeout read-timeout write-timeout)
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
   - flag & mode: passed to `isys:%sys-open',
     (logior isys:o-rdwr isys:o-nonblock) & *default-open-mode* by default.
   - external-format: see babel manual.
   - stream read/write timeout values: no default values are specified.


   Example (if rx of /dev/ttyUSB0 connected with rx should return T):
   (with-serial-stream (tty \"/dev/ttyUSB0\"   
                            :speed :b57600     
                            :parity :n)      
     (let* ((out \"hello\")                    
            (ln (length out))                
            (in (make-string ln)))           
       (setf (itty:read-timeout tty) .1)     
       (stream-write-sequence tty out 0 ln)  
       (stream-read-sequence tty in 0 ln)    
       (string= in out)))                    
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
     ,(when timeout
            `(setf (read-timeout ,stream) ,timeout)
            `(setf (read-timeout ,stream) ,timeout))
     ,(when read-timeout
            `(setf (read-timeout ,stream) ,read-timeout))
     ,(when write-timeout
            `(setf (write-timeout ,stream) ,write-timeout))
     ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-serial-streams (binds &body body)
  "Multiple `with-serial-streams` variant"
  (if binds
      `(with-serial-stream ,(car binds)
         (with-serial-streams ,(cdr binds)
           ,@body))
      `(progn ,@body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
