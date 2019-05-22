(defpackage :chipmunk
  (:use :cl)
  (:shadow :position :step :remove)
  (:export

   :make-space :free-space
   :gravity :step :add :remove

   :body

   :make-segment-shape :make-circle-shape :make-box-shape :free-shape
   :friction


   :moment-for-circle :moment-for-box

   :make-body :make-kinematic-body :make-static-body
   :free-body :velocity :position

   :register-collision-type :collision-type-name-to-value :collision-type-value-to-name
   :clear-collision-types
   :collision-type :collision-handler-for :with-collision-handler-for


   :register-shape-filter-category :shape-filter-category-name-to-value :shape-filter-category-value-to-name
   :clear-shape-filter-categories
   :categories :has-category?
   :mask :mask-has-category?
   :make-shape-filter :shape-filter

   :user-data

   :begin-collision-fun :define-collision-begin-callback

   :shapes))
(in-package :chipmunk)

(cffi:define-foreign-library chipmunk
  (:windows "chipmunk.dll")
  (:unix (:or "libchipmunk.so.7.0.2" "libchipmunk.so.7" "libchipmunk.so"))
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

(defgeneric remove (space what))
(defmethod remove ((space chipmunk.autowrap:cp-space) (shape chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-space-remove-shape space shape))
(defmethod remove ((space chipmunk.autowrap:cp-space) (body chipmunk.autowrap:cp-body))
  (chipmunk.autowrap:cp-space-remove-body space body))

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

(defun make-kinematic-body () (chipmunk.autowrap:cp-body-new-kinematic))

(defun make-static-body () (chipmunk.autowrap:cp-body-new-static))

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

(defparameter *next-collision-type-num* 0)
(defparameter *collision-types* (make-hash-table))

(defun register-collision-type (type-name)
  "Registers a collision type under as type-name, for use in collision-type and (setf collision-type).
   Collision types are used to trigger callbacks when some objects collide"
  (when (gethash type-name *collision-types*)
    (error "Collision type ~A already registered" type-name))
  (setf (gethash type-name *collision-types*) (incf *next-collision-type-num*)))

(defun clear-collision-types ()
  "Clears the stored collision types table and reset the value counter"
  (setf *collision-types* (make-hash-table))
  (setf *next-collision-type-num* 0))

(defun collision-type-name-to-value (type-name)
  "Returns a value corresponding to the collision type"
  (gethash type-name *collision-types*))

(defun collision-type-value-to-name (type-value)
  "Returns a name corresponding to collision type value"
  (loop for k being the hash-keys of *collision-types*
        using (hash-value v)
        when (= v type-value) return k))

(defmethod collision-type ((shape chipmunk.autowrap:cp-shape))
  (gethash
   (collision-type-value-to-name (chipmunk.autowrap:cp-shape-get-collision-type shape))
   *collision-types*))

(defmethod (setf collision-type) (type-name (shape chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-shape-set-collision-type
   shape
   (collision-type-name-to-value type-name)))


(defgeneric user-data (from))

(defgeneric (setf user-data) (data from))

(defmethod user-data ((from chipmunk.autowrap:cp-shape))
  "Stores a pointer to the user-data part of the shape. The argument should be a CFFI pointer."
  (chipmunk.autowrap:cp-shape-get-user-data from))

(defmethod (setf user-data) (data (from chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-shape-set-user-data from data))

(defmethod user-data ((from chipmunk.autowrap:cp-body))
  (chipmunk.autowrap:cp-body-get-user-data from))

(defmethod (setf user-data) (data (from chipmunk.autowrap:cp-body))
  (chipmunk.autowrap:cp-body-set-user-data from data))

(defun collision-handler-for (space collision-type-a &optional collision-type-b)
  "Returns or creates a collision handler for collision type names a and b, or for type name a and everything else"
  (if collision-type-b
      (chipmunk.autowrap:cp-space-add-collision-handler
       space
       (collision-type-name-to-value collision-type-a)
       (collision-type-name-to-value collision-type-b))
      (chipmunk.autowrap:cp-space-add-wildcard-handler
       space
       (collision-type-name-to-value collision-type-a))))

(defmacro with-collision-handler-for
    ((handler-name (space collision-type-a &optional collision-type-b)) &body body)
  "Create and bind a collision handler for the provided types, under the specified name"
  `(let ((,handler-name (collision-handler-for ,space ,collision-type-a ,collision-type-b)))
     ,@body))

(defmethod begin-collision-fun ((handler chipmunk.autowrap:cp-collision-handler))
  (chipmunk.autowrap:cp-collision-handler.begin-func handler))

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
     (let ((result
             (let ((,arbiter-var
                     (autowrap:make-wrapper-instance 'chipmunk.autowrap:cp-arbiter :ptr arbiter-ptr))
                   (,space-var
                     (autowrap:make-wrapper-instance 'chipmunk.autowrap:cp-space :ptr space-ptr))
                   ;; If the user chose to declare data as ignorable, it'd be in the same scope and
                   ;; the compiler will not issue a warning
                   (,data-var data-ptr))
               ,@body)))
       (ecase result ;; Check that the function returns either 0 or 1
         ((0 1) result)))))

;; The first value will be 0 after incf
(defparameter *next-collision-filter-power* -1)

(defparameter *shape-filter-categories* (make-hash-table))

(defun clear-shape-filter-categories ()
  (setf *next-collision-filter-power* -1)
  (setf *shape-filter-categories* (make-hash-table)))

(defun register-shape-filter-category (category-name)
  (let ((category-number (expt 2 (incf *next-collision-filter-power*))))

    (when (gethash category-name *shape-filter-categories*)
      (error "Shape filter category ~A already registered" category-name))

    (setf (gethash category-name *shape-filter-categories*) category-number)))

(defun shape-filter-category-name-to-value (category-name)
  "Returns a value corresponding to the collision type"
  (gethash category-name *shape-filter-categories*))

(defun shape-filter-category-value-to-name (category-value)
  "Returns a name corresponding to collision type value"
  (loop for k being the hash-keys of *shape-filter-categories*
        using (hash-value v)
        when (= v category-value) return k))

(defmethod categories ((filter chipmunk.autowrap:cp-shape-filter))
  "Returns the number value of categories (use has-category? if you want to check
   if the object is in a category)"
  (chipmunk.autowrap:cp-shape-filter.categories filter))

(defmethod (setf categories) (category-names-or-all (filter chipmunk.autowrap:cp-shape-filter))
  "Using category names or :all, sets the categories the filter belongs to"
  (setf (chipmunk.autowrap:cp-shape-filter.categories filter)
        (case category-names-or-all
          (:all chipmunk.autowrap:+cp-all-categories+)
          (otherwise
           (let ((category-value-list (mapcar #'shape-filter-category-name-to-value category-names-or-all)))
             (apply #'logior category-value-list))))))

(defmethod has-category? ((filter chipmunk.autowrap:cp-shape-filter) category-name)
  "Checks if the filter category is in the category named by category-name"
  (let ((category-value (shape-filter-category-name-to-value category-name))
        (categories (categories filter)))
    (not (zerop (logand categories category-value)))))

(defmethod mask ((filter chipmunk.autowrap:cp-shape-filter))
  "Returns the number value of the filter mask (use mask-has-category? if you want to check
   if the mask has a category)"
  (chipmunk.autowrap:cp-shape-filter.mask filter))

(defmethod (setf mask) (category-names-or-all (filter chipmunk.autowrap:cp-shape-filter))
  "Using category names, Sets the mask the filter uses"
  (setf (chipmunk.autowrap:cp-shape-filter.mask filter)
        (case category-names-or-all
          (:all chipmunk.autowrap:+cp-all-categories+)
          (otherwise
           (let ((category-value-list (mapcar #'shape-filter-category-name-to-value category-names-or-all)))
             (apply #'logior category-value-list))))))

(defmethod mask-has-category? ((filter chipmunk.autowrap:cp-shape-filter) category-name)
  "Checks if the filter mask has the category named by category-name"
  (let ((category-value (shape-filter-category-name-to-value category-name))
        (mask (mask filter)))
    (not (zerop (logand mask category-value)))))

;; TODO: groups

(defun make-shape-filter (category-names-or-all mask-category-names-or-all)
  (let* ((alloced-obj (chipmunk.wrapper::new-collected
                          (autowrap:alloc 'chipmunk.autowrap:cp-shape-filter))))

    (setf (categories alloced-obj) category-names-or-all)
    (setf (mask alloced-obj) mask-category-names-or-all)
    (setf (chipmunk.autowrap:cp-shape-filter.group alloced-obj)
          chipmunk.autowrap:+cp-no-group+)

    alloced-obj))

(defmethod (setf shape-filter) ((filter chipmunk.autowrap:cp-shape-filter) (shape chipmunk.autowrap:cp-shape))
  (chipmunk.autowrap:cp-shape-set-filter shape filter))

(defmethod shape-filter ((shape chipmunk.autowrap:cp-shape))
  (let ((filter (make-shape-filter '() '())))
    (chipmunk.autowrap:cp-shape-get-filter filter shape)))

