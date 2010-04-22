(defpackage #:cl-num-utils
    (:nicknames #:clnu)
  (:use #:cl #:cl-utilities #:iterate
        #:metabang-bind #:anaphora)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; seq

   vector* numseq vector-satisfies?

   ;; pretty

   real-epsilon *default-min-step-correction* pretty-step

   ;; bins

   bin-range bin-index bin-function bin-domain-error
   evenly-distributed-bins pretty-bins integer-bins sturges-bins 
   irregular-bins within-breaks? breaks

   ;; statistics

   size sum sse mean variance

   ))
