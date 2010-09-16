(defpackage #:cl-num-utils
    (:nicknames #:clnu)
  (:use #:cl #:cl-utilities #:iterate #:metabang-bind #:anaphora
        #:tpapp-utils)
  (:shadowing-import-from :iterate #:collecting #:collect)
  (:export

   ;; misc
   
   divides? square nif anif bic xor multf as-integer common-supertype round*

   ;; seq-and-array

   vector* iseq numseq vector-satisfies? cumsum sort-order make-similar-vector
   make-similar-array rep concat displace-array group

   ;; pretty

   real-epsilon *default-min-step-correction* pretty-step

   ;; bins

   bin-range bin-index bin-function bin-domain-error
   evenly-distributed-bins pretty-bins integer-bins sturges-bins 
   irregular-bins within? breaks

   ;; statistics

   size sum sse mean sample-var sample-sd sample-cov sample-corr

   ;; interval
   
   interval interval-left interval-right make-interval forced-interval
   make-forced-interval interval-diff interval-width interval-midpoint
   positive-interval? negative-interval? weakly-positive-interval?
   weakly-negative-interval? zero-interval? flip-interval interval-abs
   make-interval-or-nil range combined-range interval-intersection
   fraction proper-fraction? fraction-value fractions percent percents spacer
   spacer-value spacers split-interval extend-interval

   ;; histogram

   histogram-counter with-histogram-counter histogram-count
   hash-key-range hash-key-range2 hash-table->array hash-table->array2

   ;; sub

   nrow ncol sub sub-incompatible-dimensions sub-invalid-array-index 
   si cat rev  bit-vector-positions bitmap resolve-index-specification
   row-major-coefficients column-major-coefficients drop-dimensions
   index-specification-dimension with-indexing with-indexing* map-columns
   map-rows transpose create collect-rows collect-vector reshape
   reshape-calculate-dimensions rows columns pref which

   ;; ix
   
   ix expanded-keys ix-size make-ix ix->labels ix->specification ix*

   ;; optimization
   
   reached-max-iter golden-section-minimize

   ))
