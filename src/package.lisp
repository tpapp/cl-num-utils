(defpackage cl-num-utils
  (:nicknames clnu)
  (:use cl iterate let-plus anaphora alexandria)
  (:shadow mean variance median)
  (:export

   ;; macros
   
   silent check-types define-with-multiple-bindings make-keyword+ gensym+
   lazy-let* unlessf setf-nil expanding

   ;; misc
   
   nonnegative? nonpositive? divides? square nif anif bic multf as-integer 
   common-supertype round* maybe-copy-array convex-combination vector-last
   common common-length common-dimension == *==-tolerance* format-number
   ignore-error ignore-nil text-progress-bar within? fixnum?
   simple-fixnum-vector define-indirect-accessors keys-and-values thin thin-to
   log10 log2

   ;; arithmetic

   numseq ivec sum product cumulative-sum cumulative-product same-sign? 
   absolute-square

   ;; array

   first* second* third* fourth* fifth* sixth* seventh* eighth* ninth* tenth*
   nrow ncol elements square? matrix row-major-loop
   array-element-type-available displace-array make-similar-array filled-array
   rep reshape flatten-array subarrays subarray partition subvector combine
   map1

   ;; map-subarrays
   
   as-array diagonal transpose transpose* valid-permutation? permute 
   as-row as-column outer* norm1 norm2 normsup

   ;; pretty

   real-epsilon *default-min-step-correction* pretty-step

   ;; bins

   bin-index bin-locations bin-location even-bins even-bins-p pretty-bins
   integer-bins integer-bins-p
   
   format-bin-location binary-search

   ;; statistics
   
   add pool pool* conforming-accumulator with-accumulator sweep
   define-conforming-accumulator tally sample-ratio mean sse variance sd
   quantile quantiles median sum product tallier sample-ratio-accumulator
   mean-accumulator incf-mean array-mean-accumulator mean-sse-accumulator

   covariance-accumulator covariance correlation covariance-xy correlation-xy
   autocovariance-accumulator autocovariances autocorrelations lags
   
   sorting-accumulator at at-object at-subscripts sparse-accumulator-array ref
   limits residual-pair acf-accumulator

   histogram-accumulator bins locations-and-tallies location-limits
   *frequency-print-width* scott-rule histogram1

   subranges demean empirical-quantile

   ;; interval
     
   interval interval-left interval-right interval &interval &interval-r/o
   interval-diff interval-width interval-midpoint positive-interval?
   negative-interval? weakly-positive-interval? weakly-negative-interval?
   zero-interval? flip-interval interval-abs interval-or-nil limits
   interval-intersection relative relative-fraction shrink-interval spacer
   spacer-value split-interval

   ;; elementwise

   emap-dimensions emap-next recycle recycled-vector emap e+ e- e* e/ e2+ e2-
   e2* e2/ e1- e1/ eexpt eexp elog esqrt econjugate ereduce emax emin

   ;; sub

   sub asub sub-incompatible-dimensions sub-invalid-array-index incl cat rev
   ivec* sub-resolve-index sub-resolve-selection 

   positions mask which bit-to-boolean boolean-to-bit bracket

   ;; elementwise

   emap-dimensions emap-next emap emap-common-numeric-type emap-type-of e+ e-
   e* e/ e2+ e2- e2* e2/ eexpt elog esqrt ereduce emin emax stack-dimensions
   stack-into stack* stack concat* concat

   ;; utilities
   

   ;; layout
   
   layout-length layout-ref layout-position flatten-using-layout array-layout
   dictionary-layout atomic-dictionary-layout shifted-vector-layout

   ;; data-frame

   data-frame make-data-frame layout w/keys sub-rows

   ;; interaction
   
   interaction

   ;; optimization
   
   reached-max-iter  golden-section-minimize

   ;; differentiation
   
   differentiate
   #:derivative
   #:elasticity))
