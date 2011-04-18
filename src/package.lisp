(defpackage cl-num-utils
  (:nicknames clnu)
  (:use cl iterate metabang-bind anaphora alexandria)
  (:shadow mean variance)
  (:export

   ;; macros
   
   silent check-types define-with-multiple-bindings concatenate-as-strings
   make-symbol-in make-symbol* make-keyword* gensym* define-make-symbol% lazy-let*

   ;; misc
   
   nonnegative? nonpositive? divides? square nif anif bic multf as-integer 
   common-supertype round* convex-combination vector-last common common-length
   common-dimension

   ;; seq-and-array

   vector* iseq numseq vector-satisfies? map-array sum cumulative-sum sort-order
   make-similar-vector make-similar-array rep concat displace-array flatten-array
   displace-subarray group positions which which-positions which-rows 
   first* second* third* fourth* fifth* sixth* seventh* eighth* ninth* tenth*
   as-array

   ;; pretty

   real-epsilon *default-min-step-correction* pretty-step

   ;; bins

   even-bins pretty-bins integer-bins sturges-bins within? irregular-bins
   binned-data indexes bin-limit bin-origin bin-origins continuous-binned-data
   breaks bin-using-breaks bin-using-quantiles discrete-binned-data keys
   bin-discrete

   ;; statistics

   mean variance mean-and-variance weighted-mean weighted-variance
   weighted-mean-and-variance sample-quantiles

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
   with-indexing with-indexing* 

   ;; array

   map-columns map-rows transpose create collect-rows collect-vector reshape
   reshape-calculate-dimensions rows columns pref filter-rows with-filter-rows
   dot outer norm1 norm2 normsup

   ;; ix
   
   ix ix-start ix-end ix-key ix-keys hashed-index make-hashed-index
   resolve-ix-index-specification

   ;; data-frame

   data-frame make-data-frame data-frame-matrix data-frame-column-index
   with-filter-data-frame

   ;; interaction
   
   interaction

   ;; optimization
   
   reached-max-iter golden-section-minimize

   ))
