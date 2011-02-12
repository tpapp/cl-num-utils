(defpackage #:cl-num-utils
    (:nicknames #:clnu)
  (:use #:cl #:iterate #:metabang-bind #:anaphora #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:collect)
  (:shadow #:mean)
  (:export

   ;; misc
   
   divides? square nif anif bic xor multf as-integer common-supertype round*
   convex-combination vector-last

   ;; seq-and-array

   vector* iseq numseq vector-satisfies? cumsum sort-order make-similar-vector
   make-similar-array rep concat displace-array displace-subarray group positions
   which which-positions which-rows 

   ;; pretty

   real-epsilon *default-min-step-correction* pretty-step

   ;; bins

   even-bins pretty-bins integer-bins sturges-bins within? irregular-bins
   binned-data indexes bin-limit bin-origin continuous-binned-data breaks
   bin-using-breaks bin-using-quantiles discrete-binned-data keys bin-discrete

   ;; statistics

   size sum sse mean sample-var sample-sd sample-cov sample-corr
   sample-quantiles

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
   si cat rev resolve-index-specification row-major-coefficients
   column-major-coefficients drop-dimensions index-specification-dimension
   with-indexing with-indexing* map-columns map-rows transpose create
   collect-rows collect-vector reshape reshape-calculate-dimensions rows columns
   pref filter-rows with-filter-rows

   ;; ix
   
   ix ix-start ix-end ix-key ix-keys hashed-index make-hashed-index

   ;; data-frame

   data-frame make-data-frame data-frame-matrix data-frame-column-index
   with-filter-data-frame

   ;; interaction
   
   interaction

   ;; optimization
   
   reached-max-iter golden-section-minimize

   ))
