(defpackage #:cl-num-utils
    (:nicknames #:clnu)
  (:use #:cl #:cl-utilities #:iterate #:metabang-bind #:anaphora
        #:tpapp-utils)
  (:shadowing-import-from :iterate #:collecting #:collect)
  (:export

   ;; misc
   
   divides? square nif anif bic xor multf as-integer

   ;; seq

   vector* numseq vector-satisfies? cumsum sort-order make-similar-vector
   make-similar-array rep

   ;; pretty

   real-epsilon *default-min-step-correction* pretty-step

   ;; bins

   bin-range bin-index bin-function bin-domain-error
   evenly-distributed-bins pretty-bins integer-bins sturges-bins 
   irregular-bins within? breaks

   ;; statistics

   size sum sse mean sample-var sample-cov

   ;; interval
   
   interval interval-left interval-right make-interval forced-interval
   make-forced-interval interval-diff interval-width interval-midpoint
   positive-interval? negative-interval? weakly-positive-interval?
   weakly-negative-interval? zero-interval? flip-interval 
   make-interval-or-nil range combined-range interval-intersection
   extend-interval fraction fraction-value fractions percent percents spacer
   spacer-value spacers split-interval

   ;; histogram

   histogram-counter with-histogram-counter histogram-count
   hash-key-range hash-key-range2 hash-table->array hash-table->array2

   ;; sub

   nrow ncol sub-incompatible-dimensions sub-invalid-array-index 
   sub-invalid-range transform-index sub-all sub-index sub-range
   transform-range transform-ranges row-major-coefficients
   column-major-coefficients drop-dimensions range-dimension
   range-dimensions with-range-indexing sub map-columns map-rows
   transpose create collect-rows collect-vector

   ;; ix
   
   ix ix-keys make-ix conforming-ix flatten-ix ix->spec sub-ix

   ;; optimization
   
   reached-max-iter golden-section-minimize

   ))
