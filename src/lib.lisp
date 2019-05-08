(defpackage :chipmunk
  (:use :cl :plus-c)
  (:shadow :position :step)
  (:export

   :make-space :free-space
   :gravity :step :add

   :body

   :make-segment-shape :make-circle-shape :make-box-shape :free-shape
   :friction


   :moment-for-circle :moment-for-box

   :make-body :free-body :velocity :position

   :collision-type :collision-handler-for :with-collision-handler-for

   :begin-collision-fun :define-collision-begin-callback

   :shapes))
(in-package :chipmunk)

(cffi:define-foreign-library chipmunk
  (:windows "chipmunk.dll")
  (:unix "libchipmunk.so.7")
  (t (:default "libchipmunk")))
(cffi:use-foreign-library chipmunk)

(chipmunk.wrapper:define-wrapper-helpers chipmunk.autowrap:cp-vect (x y))

(defun make-space () (chipmunk.autowrap:cp-space-new))
(defun free-space (space) (chipmunk.autowrap:cp-space-free space))

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
  (autowrap:make-wrapper-instance
   'chipmunk.autowrap:cp-shape
   :ptr (chipmunk.autowrap:cp-segment-shape-new body a b radius)))

(defun make-circle-shape (body radius offset)
  (autowrap:make-wrapper-instance
   'chipmunk.autowrap:cp-shape
   :ptr (chipmunk.autowrap:cp-circle-shape-new body radius offset)))

(defun make-box-shape (body width height radius)
  ;; TODO: figure out why box returns a wrapper but other shape creators don't
  ;; (probably libffi related, since they require pass-by-value)
  (chipmunk.autowrap:cp-box-shape-new body width height radius))

(defun free-shape (shape)
  (chipmunk.autowrap:cp-shape-free shape))

(defmethod friction ((shape chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-shape-get-friction shape))

(defmethod (setf friction) (friction (shape chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-shape-set-friction shape friction))

(defun moment-for-circle (mass inner-diameter outer-diameter offset)
  (chipmunk.autowrap:cp-moment-for-circle mass inner-diameter outer-diameter offset))

(defun moment-for-box (mass width height)
  (chipmunk.autowrap:cp-moment-for-box mass width height))

(defun make-body (mass moment) (chipmunk.autowrap:cp-body-new mass moment))

(defun free-body (body) (chipmunk.autowrap:cp-body-free body))

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


(defmethod collision-type ((shape chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-shape-get-collision-type shape))

(defmethod (setf collision-type) (type (shape chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-shape-set-collision-type shape type))


(defun collision-handler-for (space collision-type-a &optional collision-type-b)
  "Returns or creates a collision handler for type a and b, or for type a and everything else"
  (if collision-type-b
      (chipmunk.autowrap:cp-space-add-collision-handler space collision-type-a collision-type-b)
      (chipmunk.autowrap:cp-space-add-wildcard-handler space collision-type-a)))

(defmacro with-collision-handler-for
    ((handler-name (space collision-type-a &optional collision-type-b)) &body body)
  "Create and bind a collision handler for the provided types, under the specified name"
  `(let ((,handler-name (collision-handler-for ,space ,collision-type-a ,collision-type-b)))
     ,@body))

(defmethod begin-collision-fun ((handler chipmunk.autowrap:cp-collision-handler)))

(defmethod (setf begin-collision-fun) (fun (handler chipmunk.autowrap:cp-collision-handler))
  (setf (chipmunk.autowrap:cp-collision-handler.begin-func handler) (autowrap:callback fun)))

(defmethod shapes ((arbiter chipmunk.autowrap:cp-arbiter))
  "Returns the colliding shapes from the arbiter"
  (autowrap:with-many-alloc ((a-ptr '(:pointer chipmunk.autowrap:cp-shape))
                             (b-ptr '(:pointer chipmunk.autowrap:cp-shape)))
    (chipmunk.autowrap:cp-arbiter-get-shapes arbiter a-ptr b-ptr)

    (let ((a-s (autowrap:make-wrapper-instance 'chipmunk.autowrap:cp-shape
                                               :ptr
                                               (cffi:mem-ref a-ptr :pointer)))
          (b-s (autowrap:make-wrapper-instance 'chipmunk.autowrap:cp-shape
                                               :ptr
                                               (cffi:mem-ref b-ptr :pointer))))
      (values a-s b-s))))

(defmacro define-collision-begin-callback (name (arbiter-var space-var data-var) &body body)
  "Defines a callback intended to be passed to the begin-collision-fun of a collision handler.
   This function MUST return either 1 or 0 to determine whether the collision should occur or not"
  `(autowrap:defcallback ,name :int
       ((arbiter-ptr :pointer)
        (space-ptr :pointer)
        (data-ptr :pointer))
     (let ((,arbiter-var
             (autowrap:make-wrapper-instance 'chipmunk.autowrap:cp-arbiter :ptr arbiter-ptr))
           (,space-var
             (autowrap:make-wrapper-instance 'chipmunk.autowrap:cp-space :ptr space-ptr))
           ;; If the user chose to declare data as ignorable, it'd be in the same scope and
           ;; the compiler will not issue a warning
           (,data-var data-ptr))
       ,@body)))

