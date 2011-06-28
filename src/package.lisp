(defpackage cl-num-utils
  (:nicknames clnu)
  (:use cl iterate let-plus anaphora alexandria)
  (:shadow mean variance flatten)
  (:export

   ;; macros
   
   silent check-types define-with-multiple-bindings concatenate-as-strings
   make-symbol-in make-symbol* make-keyword* gensym* define-make-symbol%
   lazy-let* unlessf

   ;; misc
   
   nonnegative? nonpositive? divides? square nif anif bic multf as-integer 
   common-supertype round* convex-combination vector-last common common-length
   common-dimension == *==-tolerance* format-number ignore-error ignore-nil
   text-progress-bar within? fixnum? simple-fixnum-vector

   ;; arithmetic

   numseq ivec sum product cumulative-sum cumulative-product

   ;; array

   first* second* third* fourth* fifth* sixth* seventh* eighth* ninth* tenth*
   nrow ncol elements square? matrix row-major-loop
   array-element-type-available displace-array make-similar-array filled-array
   rep reshape flatten-array subarrays subarray subvector combine map1
   ;; map-subarrays
   
   as-array diagonal transpose transpose* valid-permutation? permute 
   as-row as-column dot outer norm1 norm2 normsup

   ;; pretty

   real-epsilon *default-min-step-correction* pretty-step

   ;; bins

   bin-index bin-locations bin-location even-bins even-bins-p pretty-bins
   sturges-bins integer-bins integer-bins-p add-observation total-frequency
   frequency relative-frequency subscript-limits subscript-limit
   subscript-rank hashed-frequencies *frequency-print-width*
   histogram-locations hashed-histogram make-hashed-histogram
   histogram-from-sequence histogram-from-matrix binary-search

   ;; statistics
   
   add pool pool* conforming-accumulator with-accumulator sweep
   define-conforming-accumulator tally mean sse variance quantile sum product 
   tallier mean-accumulator incf-mean array-mean-accumulator
   mean-sse-accumulator

   covariance-accumulator covariance correlation covariance-xy correlation-xy
   autocovariance-accumulator autocovariances autocorrelations lags
   
   sorting-accumulator @ sparse-accumulator-array ref limits residual-pair
   acf-accumulator

   subranges
     
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

   sub sub-incompatible-dimensions sub-invalid-array-index incl cat rev ivec*
   sub-resolve-index sub-resolve-selection 

   positions mask which

   ;; elementwise

   emap-dimensions emap-next emap emap-common-numeric-type emap-type-of e+ e-
   e* e/ e2+ e2- e2* e2/ eexpt elog esqrt ereduce emin emax stack-dimensions
   stack-into stack* stack concat* concat

   ;; utilities
   
   demean

   ;; layout
   
   layout-length layout-ref layout-position array-layout dictionary-layout
   atomic-dictionary-layout shifted-vector-layout

   ;; data-frame

   data-frame make-data-frame layout w/keys sub-rows

   ;; interaction
   
   interaction

   ;; optimization
   
   reached-max-iter golden-section-minimize

   ))
