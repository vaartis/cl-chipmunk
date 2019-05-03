(defpackage :chipmunk
  (:use :cl :plus-c))
(in-package :chipmunk)

(cffi:define-foreign-library chipmunk
  (:windows "chipmunk.dll")
  (:unix "libchipmunk.so.7")
  (t (:default "libchipmunk")))
(cffi:use-foreign-library chipmunk)

(chipmunk.wrapper:define-wrapper-helpers chipmunk.autowrap:cp-vect (x y))

;; (defun make-space ()
;;   (c-let ((new-space chipmunk.autowrap:cp-space
;;                      :from (c-fun chipmunk.autowrap:cp-space-new)))
;;     (let ((ptr (autowrap:ptr new-space)))
;;       (tg:finalize new-space (lambda () (c-fun chipmunk.autowrap:cp-space-free ptr)))
;;
;;       (c-fun chipmunk.autowrap:cp-space-set-gravity new-space (slot-value (make-vect :x 1.0d0 :y -10.0d0) 'chipmunk.wrapper:wrapper))
;;
;;       (let ((s (make-instance 'space)))
;;         (setf (slot-value s 'wrapper) new-space)
;;         s))))
;;
;; (defmethod gravity ((space space))
;;   (with-slots (wrapper) space
;;     (c-let ((got-gravity chipmunk.autowrap:cp-vect))
;;       (c-fun chipmunk.autowrap:cp-space-get-gravity got-gravity wrapper)
;;
;;       (let ((g (make-instance 'vect)))
;;         (setf (slot-value g 'wrapper) got-gravity)))))
;;
;; (defmethod (setf gravity) ((gravity vect) (space space))
;;   (with-slots ((space-w wrapper)) space
;;     (with-slots ((gravity-w chipmunk.wrapper:wrapper)) gravity
;;       (c-fun chipmunk.autowrap:cp-space-set-gravity space-w gravity-w))))
