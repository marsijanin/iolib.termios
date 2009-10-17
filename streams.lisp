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
  (with-tty-stream (tty \"/dev/ttyUSB0\" (logior o-rdwr      
                                               o-nonblock) 
                        'raw '(parity nil) 'b115200        
                        '(vmin 0) '(vtime 0))              
    (let* ((out \"hello\")                                   
           (ln (length out))                               
           (in  (make-string ln)))                         
      (stream-write-sequence tty out 0 ln)                 
      (sleep .1)                                           
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
(defmacro with-raw-serial ((stream path
                                   &key (speed 115200)
                                   (parity :n)
                                   (byte-size 8))
                           &body body)
  "Wraper around `with-tty-stream' with a very few number of &key options:
   - speed: baud rate. An integer, this macro will look for a corresponding
     baud rate constant and use it, or signal an error otherwise;
   - parity: one of `:n' (no parity checking), `:e' (even), `:o' (odd),
     or `:s' (space parity);
   - byte-size: character size as it described in
     `posix serial programming manual', can be an integer in 5-8 (inclusive)
     diapason.

     I.e. 9600-8n1 mode will be `:speed 9600 :parity :n :byte-size 8',
     and 300-5e1 `:speed 300 :parity :e :byte-size 5'.

     115200-8n1 is default.
  "
  `(with-tty-stream
       (,stream ,path (logior o-rdwr o-nonblock)
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
                          "Unsupported parity checking mode ~a" parity)))
                ;; byte size
                ,@(case byte-size
                       (5 `('(cs5 t)))
                       (6 `('(cs6 t)))
                       (7 `('(cs7 t)))
                       (8 `('(cs8 t)))
                       (t (error "Byte size ~a is unsupported" byte-size))))
     ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
