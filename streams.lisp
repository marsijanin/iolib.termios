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
(defun open-serial-stream (path &key (flag (logior o-rdwr o-nonblock))
                        (mode *default-open-mode*)
			(external-format :default))
  "Return `dual-channel-tty-gray-stream' instances associated
   with serial device"
  (let ((fd (%sys-open path flag mode))
        (termios (foreign-alloc 'termios)))
    (%tcgetattr fd termios)
    (make-instance 'dual-channel-tty-gray-stream
		   :input-fd fd
		   :output-fd fd
		   :path path
		   :external-format external-format
                   :original-settings termios)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Close method for dual-channel-single-fd-gray-stream in iolib.streams,
;; (which call %sys-close) is :around too. 
(defmethod close :around ((stream dual-channel-tty-gray-stream) &key abort)
  (declare (ignorable abort))
  (when (fd-of stream)
    (%tcsetattr (fd-of stream) tcsanow (original-settings stream))
    (foreign-free (original-settings stream))
    (setf (slot-value stream 'original-settings) nil))
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
                                      &key (speed 115200)
                                      (parity :n)
                                      (byte-size 8)
                                      ;; not an posix option
                                      #+(or bsd linux)hardware-flow-control
                                      software-flow-control
                                      (flag (logior o-rdwr o-nonblock))
                                      (mode *default-open-mode*)
                                      (external-format :default)
                                      timeout read-timeout write-timeout)
                              &body body)
  "Wrapper around `with-tty-stream' with a few number of settings:
   - speed: baud rate. An integer, this macro will look for a corresponding
     baud rate constant and use it, or signal an error otherwise;
   - parity: one of `:n' (no parity checking), `:e' (even), `:o' (odd),
     or `:s' (space parity);
   - byte-size: character size as it described in
     `posix serial programming manual', can be an integer in 5-8 (inclusive)
     diapason;
   - `hardware/software-flow-control' for enable hardware/software flow control
     (disable by default). Note, what hardware flow control (crtscts) is not an
     posix feature, so it can be not implemented on some platforms.

     I.e. 9600-8n1 mode will be `:speed 9600 :parity :n :byte-size 8',
     and 300-5e1 `:speed 300 :parity :e :byte-size 5'. 115200-8n1 is default.

   Other &key parameters is:
   - flag & mode: passed to `isys:%sys-open',
     (logior isys:o-rdwr isys:o-nonblock) & *default-open-mode* by default.
   - external-format: see babel manual.
   - stream read/write timeout values: no default values are specified.


   Example (if rx of /dev/ttyUSB0 connected with rx should return T):
   (with-serial-stream (tty \"/dev/ttyUSB0\"   
                            :speed 57600     
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
     (stty (fd-of ,stream) 
           ;; speed first
           ,@(let* ((sym (find-symbol (format nil "B~a" speed)
                                      :iolib.termios))
                    (hash (gethash sym *termios-options*)))
                   (if (and hash (eql (cdr hash) 'baud-rates))
                       `(',sym)
                       (error "Unknown baud rate ~a" speed)))
           ;; do not block on reading
           'raw '(vtime 0) '(vmin 0)
           ;; allways reset csize before set byte size
           'csize
           ;; parity
           ,@(case parity
                   (:n `('parenb 'cstopb))
                   (:e `('(parenb t) 'parodd 'cstopb))
                                 (:o `('(parenb t) '(parodd t) 'cstopb))
                                 (:s `('parenb 'parodd 'cstopb))
                                 (t
                                  (error
                                   "Unsupported parity checking mode ~a"
                                   parity)))
           ;; byte size
           ,@(case byte-size
                   (5 `('(cs5 t)))
                   (6 `('(cs6 t)))
                   (7 `('(cs7 t)))
                   (8 `('(cs8 t)))
                   (t (error "Byte size ~a is unsupported" byte-size)))
           ;; hardware flow control
           #+(or bsd linux),@(when hardware-flow-control `('(crtscts t)))
           ,@(when software-flow-control `('(ixon t) '(ixoff t))))
     ,(when timeout
            `(setf (read-timeout ,stream) ,timeout)
            `(setf (read-timeout ,stream) ,timeout))
     ,(when read-timeout
            `(setf (read-timeout ,stream) ,read-timeout))
     ,(when write-timeout
            `(setf (write-timeout ,stream) ,write-timeout))
     ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
