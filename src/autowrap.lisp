(defpackage :chipmunk.autowrap)
(in-package :chipmunk.autowrap)

(autowrap:c-include '(chipmunk specs "chipmunk.h")
                    :exclude-sources ("/usr/include/"
                                      "/usr/lib/clang/([^/]*)/include/(?!stddef.h)")
                    :include-sources ("chipmunk")
                    :sysincludes '("/usr/lib/gcc/x86_64-unknown-linux-gnu/8.3.0/include/")
                    :exclude-definitions ("__.*")
                    :exclude-constants ("__.*")
                    :spec-path '(chipmunk specs))
