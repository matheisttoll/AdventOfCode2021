(defpackage #:aoc2021
  (:use #:common-lisp #:iterate)
  (:import-from #:uiop #:split-string)
  (:import-from #:alexandria #:if-let
                             #:when-let
                             #:read-file-into-string
                             #:curry
                             #:rcurry
                             #:compose
                             #:hash-table-alist
                             #:hash-table-keys
                             #:copy-array
                             #:maxf
                             #:minf
                             #:appendf
                             #:switch
                             #:cswitch
                             #:eswitch
                             #:map-permutations)
  (:import-from #:cl-ppcre #:split
                #:scan-to-strings))
