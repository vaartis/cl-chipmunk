(defpackage :chipmunk.autowrap)
(in-package :chipmunk.autowrap)

(autowrap:c-include '(chipmunk specs "chipmunk.h")
                    :exclude-sources ("/usr/include/"
                                      "/usr/lib/clang/([^/]*)/include/(?!stddef.h)")
                    :include-sources ("chipmunk"
                                      "stdint.h" "bits/types.h"
                                      "sys/types.h" "bits/stdint")
                    :sysincludes '("/usr/lib/gcc/x86_64-unknown-linux-gnu/8.3.0/include/")
                    :spec-path '(chipmunk specs))
