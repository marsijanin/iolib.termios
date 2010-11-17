;;;; Fix long-long support for ecl in recent cffi version (from quicklisp)

(in-package #:cffi-sys)

(setf *features* (remove 'cffi-features:no-long-long *features*))
(setf *features* (remove 'cffi-sys::no-long-long *features*))

#+uint64-t
(let ((tab (symbol-value '+translation-table+)))
  (unless (ignore-errors (cffi-type->ecl-type :long-long))
    (push '(:long-long :uint64-t "long long") tab)
    (push '(:unsigned-long-long :uint64-t "unsigned long long") tab)
    (defconstant +translation-table+ tab)))



;;; si:make-foreign-data-from-array cat return pointer
;;; to the vector data, but for a few types
(defun %vector-address (vector)
  "Return the address of VECTOR's data."
  (si:make-foreign-data-from-array vector))

;;; ECL, built with the Boehm GC never moves allocated data, so this
;;; isn't nearly as hard to do. In fact, we support a bunch of vector
;;; types that other backends don't, but such possibility avaliable
;;; only at compile time, not at run time
(define-compiler-macro %vector-address (vector)
  "Return the address of VECTOR's data."
  `(progn
     (check-type ,vector
 		 (or (vector (unsigned-byte 8))
 		     (vector (signed-byte 8))
 		     #+uint16-t (vector (unsigned-byte 16))
 		     #+uint16-t (vector (signed-byte 16))
 		     #+uint32-t (vector (unsigned-byte 32))
 		     #+uint32-t (vector (signed-byte 32))
 		     #+uint64-t (vector (unsigned-byte 64))
 		     #+uint64-t (vector (signed-byte 64))
 		     (vector single-float)
 		     (vector double-float)
 		     (vector bit)
 		     (vector base-char)
 		     #+unicode (vector character)))
     ;; ecl_array_data is a union, so we don't have to pick the specific
     ;; fields out of it, so long as we know the array has the expected
     ;; type.
     (ffi:c-inline (,vector) (object) :unsigned-long
 		   "(unsigned long) #0->vector.self.b8"
 		   :side-effects nil
 		   :one-liner t)))


(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  `(let ((,ptr-var (make-pointer (%vector-address ,vector))))
     ,@body))
;;;; EOF
