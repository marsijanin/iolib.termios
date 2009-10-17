;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; streams for devices supported termios (3p) api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.termios)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass dual-channel-tty-gray-stream (dual-channel-single-fd-gray-stream)
  ((path :reader tty-path :initarg :path :type string))
  (:documentation "Gray stream class for serial devices"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun open-tty-stream (path
			&key (flag o-rdwr) (mode *default-open-mode*)
			(external-format :default))
  "Return `dual-channel-tty-gray-stream' instances associated
   with serial device"
  (let ((fd (%sys-open path flag mode)))
    (make-instance 'dual-channel-tty-gray-stream
		   :input-fd fd
		   :output-fd fd
		   :path path
		   :external-format external-format)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-tty-stream ((stream-var path flag &rest options) &body body)
  "Trying to open serial device by `path', to setup it by specified `options'
   (same syntax as for `stty'), to bind `dual-channel-tty-gray-stream' instances
   associated with serial device to the `stream-var' variable,
   execute `body', to restore original serial device settings
   after execution of the `body' (in protected part of the
   `unwind-protect' macro) and to close stream.

  Checking serial device by Rx<->Tx connection:
  (with-tty-stream (tty \"/dev/ttyUSB0\" (logior o-rdwr o-nonblock) 
                        'really-raw '(parity nil) 'b115200)  
    (let* ((out \"hello\")                          
           (ln (length out))                      
           (in  (make-string ln)))                
      (stream-write-sequence tty out 0 ln)        
      (stream-read-sequence tty in 0 ln)          
      in))
  "
  (with-gensyms (old set)
  `(let ((,stream-var (open-tty-stream ,path :flag ,flag))
	 (,set))
     (with-foreign-object (,old 'termios)
       (unwind-protect
	    (progn
	      (%tcgetattr (input-fd-of ,stream-var) ,old)
	      (setf ,set (stty (input-fd-of ,stream-var) ,@options))
	      ,@body)
	 (progn (if (zerop ,set)
		    (%tcsetattr (input-fd-of ,stream-var) tcsanow ,old))
		(close ,stream-var)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;