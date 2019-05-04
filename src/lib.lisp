(defpackage :chipmunk
  (:use :cl :plus-c)
  (:shadow :position :step)
  (:export

   :make-space
   :gravity :step :add

   :body

   :make-segment-shape :make-circle-shape :make-box-shape
   :friction


   :moment-for-circle :moment-for-box

   :make-body :velocity :position))
(in-package :chipmunk)

(cffi:define-foreign-library chipmunk
  (:windows "chipmunk.dll")
  (:unix "libchipmunk.so.7")
  (t (:default "libchipmunk")))
(cffi:use-foreign-library chipmunk)

(chipmunk.wrapper:define-wrapper-helpers chipmunk.autowrap:cp-vect (x y))

(defun make-space ()
  (chipmunk.wrapper::new-collected (chipmunk.autowrap:cp-space-new) chipmunk.autowrap:cp-space-free))

(defmethod gravity ((space chipmunk.autowrap:cp-space))
  ;; Create an empty-ish gravity vector and assign it the return value
  (let ((got-gravity (make-cp-vect 0d0 0d0)))
    (chipmunk.autowrap:cp-space-get-gravity got-gravity space)
    got-gravity))

(defmethod (setf gravity) ((gravity chipmunk.autowrap:cp-vect) (space chipmunk.autowrap:cp-space))
  (chipmunk.autowrap:cp-space-set-gravity space gravity))

(defmethod step ((space chipmunk.autowrap:cp-space) time-step)
  (chipmunk.autowrap:cp-space-step space time-step))

(defgeneric add (space what))
(defmethod add ((space chipmunk.autowrap:cp-space) (shape chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-space-add-shape space shape)
  ;; Return the same thing passed, to mimic the original behaviour
  shape)
(defmethod add ((space chipmunk.autowrap:cp-space) (body chipmunk.autowrap:cp-body))
  (chipmunk.autowrap:cp-space-add-body space body)

  body)

(defgeneric body (from))
(defmethod body ((space chipmunk.autowrap:cp-space))
  (chipmunk.autowrap:cp-space-get-static-body space))

(defun make-segment-shape (body a b radius)
  (chipmunk.wrapper::new-collected
      (autowrap:make-wrapper-instance
       'chipmunk.autowrap:cp-shape
       :ptr (chipmunk.autowrap:cp-segment-shape-new body a b radius))
      chipmunk.autowrap:cp-shape-free))

(defun make-circle-shape (body radius offset)
  (chipmunk.wrapper::new-collected
      (autowrap:make-wrapper-instance
       'chipmunk.autowrap:cp-shape
       :ptr (chipmunk.autowrap:cp-circle-shape-new body radius offset))
      chipmunk.autowrap:cp-shape-free))

(defun make-box-shape (body width height radius)
  (chipmunk.wrapper::new-collected
      (autowrap:make-wrapper-instance
       'chipmunk.autowrap:cp-shape
       :ptr (chipmunk.autowrap:cp-box-shape-new body width height radius))
      chipmunk.autowrap:cp-shape-free))

(defmethod friction ((shape chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-shape-get-friction shape))

(defmethod (setf friction) (friction (shape chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-shape-set-friction shape friction))

(defun moment-for-circle (mass inner-diameter outer-diameter offset)
  (chipmunk.autowrap:cp-moment-for-circle mass inner-diameter outer-diameter offset))

(defun moment-for-box (mass width height)
  (chipmunk.autowrap:cp-moment-for-box mass width height))

(defun make-body (mass moment)
  (chipmunk.wrapper::new-collected (chipmunk.autowrap:cp-body-new mass moment)
      chipmunk.autowrap:cp-body-free))

(defmethod velocity ((body chipmunk.autowrap:cp-body))
  (let ((ret (make-cp-vect 0d0 0d0)))
    (chipmunk.autowrap:cp-body-get-velocity ret body)))

(defmethod (setf velocity) ((velocity chipmunk.autowrap:cp-vect) (body chipmunk.autowrap:cp-body))
  (chipmunk.autowrap:cp-body-set-velocity body velocity))

(defmethod position ((body chipmunk.autowrap:cp-body))
  (let ((result-vect (make-cp-vect 0d0 0d0)))
    (chipmunk.autowrap:cp-body-get-position result-vect body)))

(defmethod (setf position) ((new-pos chipmunk.autowrap:cp-vect) (body chipmunk.autowrap:cp-body))
  (chipmunk.autowrap:cp-body-set-position body new-pos))

