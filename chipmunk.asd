(asdf:defsystem :chipmunk
  :description "Chipmunk2D bindings"
  :author "Ekaterina Vaartis <vaartis@cock.li>"
  :license "MIT"
  :depends-on (:trivial-garbage :cl-autowrap/libffi)
  :pathname "src"
  :components
  ((:module specs
    :components
    ((:static-file "chipmunk.h")))
   (:file "autowrap")
   (:file "wrapper")
   (:file "lib")))
