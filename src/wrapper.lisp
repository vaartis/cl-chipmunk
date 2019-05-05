(defpackage :chipmunk.wrapper
  (:use :cl :cffi)
  (:export :define-wrapper-helpers))
(in-package :chipmunk.wrapper)

(defmacro new-collected (creation-form &optional (freeing-fun 'autowrap:free))
  `(autowrap:autocollect (ptr) ,creation-form
     (,freeing-fun ptr)))

(defmacro define-wrapper-accessor (for-type accessor-name)
  "Defines an accessor function for a wrapper type `for-type`,
   where the underlying wraper is of type `wrapper-type` and the field is named `accessor-name`.
   Arguments are not evaluated"
  (let ((fun-name (intern (symbol-name accessor-name)))
        (autowrap-accessor-name (intern (format nil "~A.~A" (symbol-name for-type) (symbol-name accessor-name))
                                         (package-name (symbol-package for-type)))))
    `(progn
       (defmethod ,fun-name ((o ,for-type))
         (,autowrap-accessor-name o))
       (defmethod (setf ,fun-name) (,accessor-name (o ,for-type))
         (setf (,autowrap-accessor-name o) ,accessor-name))

       (export ',fun-name))))

(defmacro define-wrapper-accessors (for-type accessor-names)
  "Defines multiple accessor with define-wrapper-accessors"
  `(progn
     ,@(mapcar
        (lambda (name) `(define-wrapper-accessor ,for-type ,name))
        accessor-names)))

(defmacro define-make-wrapper (for-type fields)
  (let ((fun-name
          (intern (format nil "MAKE-~A" (symbol-name for-type)))) )
    `(progn
       (defun ,fun-name ,fields
         (let* ((alloced-obj (new-collected (autowrap:alloc ',for-type))))

           ,@(mapcar
              (lambda (field)
                `(setf
                  (,(intern (format nil "~A.~A" (symbol-name for-type) (symbol-name field))
                            (package-name (symbol-package for-type)))
                   alloced-obj)
                  ,field))
              fields)

           alloced-obj))
       (export ',fun-name))))

(defmacro define-wrapper-helpers (wrapped-type fields)
  "Defines a type-safe-ish wrapper around the autowrap's wrapper and generates functions to access it's fields"
  `(progn
     (define-wrapper-accessors ,wrapped-type ,fields)

     (define-make-wrapper ,wrapped-type ,fields)))
