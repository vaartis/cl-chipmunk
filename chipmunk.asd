(asdf:defsystem :chipmunk
  :description "Chipmunk2D bindings"
  :author "Ekaterina Vaartis <vaartis@cock.li>"
  :license "MIT"
  :depends-on (:trivial-garbage :cl-autowrap/libffi)
  :pathname "src"
  :components
  ((:file "autowrap")
   (:module specs
    :components
    ((:static-file "chipmunk.h")))
   (:file "wrapper")
   (:file "lib")))
