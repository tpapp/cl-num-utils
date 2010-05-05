(defpackage #:cl-num-utils
    (:nicknames #:clnu)
  (:use #:cl #:cl-utilities #:iterate
        #:metabang-bind #:anaphora)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; seq

   vector* numseq vector-satisfies? cumsum

   ;; pretty

   real-epsilon *default-min-step-correction* pretty-step

   ;; bins

   bin-range bin-index bin-function bin-domain-error
   evenly-distributed-bins pretty-bins integer-bins sturges-bins 
   irregular-bins within-breaks? breaks

   ;; statistics

   size sum sse mean variance

   ;; interval
   
   interval interval-left interval-right make-interval forced-interval make-forced-interval
   interval-diff interval-width interval-midpoint positive-interval-p negative-interval-p
   weakly-positive-interval-p  weakly-negative-interval-p zero-interval-p flip-interval 
   make-interval-or-nil range combined-range interval-intersection fraction fraction-value 
   fractions percent percents spacer spacer-value spacers split-interval

   ;; histogram

   histogram-counter with-histogram-counter histogram-count hash-key-range hash-key-range2
   hash-table->array hash-table->array2

   ))
