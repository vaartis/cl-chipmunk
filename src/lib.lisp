(defpackage :chipmunk
  (:use :cl :plus-c))
(in-package :chipmunk)

(cffi:define-foreign-library chipmunk
  (:windows "chipmunk.dll")
  (:unix "libchipmunk.so.7")
  (t (:default "libchipmunk")))
(cffi:use-foreign-library chipmunk)

(chipmunk.wrapper:define-wrapper-helpers chipmunk.autowrap:cp-vect (x y))

(defun make-space ()
  (let* ((new-space (chipmunk.autowrap:cp-space-new))
         (ptr (autowrap:ptr new-space)))
    (tg:finalize new-space (lambda () (chipmunk.autowrap:cp-space-free ptr)))

    new-space))

(defmethod gravity ((space chipmunk.autowrap:cp-space))
  ;; Create an empty-ish gravity vector and assign it the return value
  (let ((got-gravity (make-cp-vect 0d0 0d0)))
    (chipmunk.autowrap:cp-space-get-gravity got-gravity space)
    got-gravity))

(defmethod (setf gravity) ((gravity chipmunk.autowrap:cp-vect) (space chipmunk.autowrap:cp-space))
  (chipmunk.autowrap:cp-space-set-gravity space gravity))
