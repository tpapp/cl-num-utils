(defpackage cl-num-utils
  (:nicknames clnu)
  (:use cl iterate metabang-bind let-plus anaphora alexandria)
  (:shadow mean variance)
  (:export

   ;; macros
   
   silent check-types define-with-multiple-bindings concatenate-as-strings
   make-symbol-in make-symbol* make-keyword* gensym* define-make-symbol%
   lazy-let* unlessf

   ;; misc
   
   nonnegative? nonpositive? divides? square nif anif bic multf as-integer 
   common-supertype round* convex-combination vector-last common common-length
   common-dimension == *==-tolerance* format-number ignore-error ignore-nil
   text-progress-bar within?

   ;; seq-and-array

   simple-fixnum-vector as-simple-fixnum-vector array* vector* filled-array
   iseq numseq vector-satisfies? map-array sumcumulative-sum sort-order
   make-similar-vector make-similar-array rep displace-array
   flatten-array subarray group positions which which-positions
   which-rows first* second* third* fourth* fifth* sixth* seventh* eighth*
   ninth* tenth* as-array row-major-loop

   ;; pretty

   real-epsilon *default-min-step-correction* pretty-step

   ;; bins

   bin-index bin-locations bin-location even-bins even-bins-p pretty-bins
   sturges-bins integer-bins integer-bins-p add-observation total-frequency
   frequency relative-frequency subscript-limits subscript-limit
   subscript-rank hashed-frequencies *frequency-print-width*
   histogram-locations hashed-histogram make-hashed-histogram
   histogram-from-sequence histogram-from-matrix

   ;; statistics
   
   add statistic conforming-accumulator sweep tally mean sse variance
   mean-and-variance quantile sum product tallier accumulator-array
   add-array-elementwise mean-accumulator mean-sse-accumulator
   sorting-accumulator @ sparse-accumulator-array ref limits residual-pair
   acf-accumulator
     
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

   ;; elementwise

   emap-dimensions emap-next recycle recycled-vector emap e+ e- e* e/ e2+ e2-
   e2* e2/ e1- e1/ eexpt eexp elog esqrt econjugate ereduce emax emin

   ;; sub

   nrow ncol matrix matrix? square? sub sub-incompatible-dimensions
   sub-invalid-array-index si cat rev resolve-index-specification 
   row-major-coefficients column-major-coefficients drop-dimensions
   index-specification-dimension with-indexing with-indexing* 

   ;; array

   array-element-type-available map-columns map-rows transpose
   transpose* create collect-rows collect-vector as-row as-column reshape
   reshape-calculate-dimensions rows columns pref filter-rows with-filter-rows
   shrink-rows dot outer norm1 norm2
   normsup

   ;; elementwise

   emap-dimensions emap-next emap emap-common-numeric-type emap-type-of e+ e-
   e* e/ e2+ e2- e2* e2/ eexpt elog esqrt ereduce emin emax stack-dimensions
   stack-into stack* stack concat* concat

   ;; ix
   
   ix-key? ix ix-start ix-end ix-key ix-keys hashed-index make-hashed-index
   resolve-ix-index-specification

   ;; data-frame

   data-frame make-data-frame data-frame-matrix data-frame-column-index
   with-filter-data-frame

   ;; interaction
   
   interaction

   ;; optimization
   
   reached-max-iter golden-section-minimize

   ))
