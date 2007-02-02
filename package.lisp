(cl:defpackage :fsvd
  (:use #:common-lisp)
  (:export #:map-matrix
           #:height-of
           #:width-of
           #:size-of
           #:map-row-densely
           #:map-column-densely
           #:do-matrix
           #:do-matrix-macro-name
           #:sv
           #:sv-left
           #:sv-right
           #:svd
           #:make-svd
           #:compact-svd
           #:save-svd
           #:load-svd
           #:svd-value
           #:make-svd-approximator
           ;; Utilities
           #:approximation-rmse
           #:limiting-supervisor
           #:svd-in-progress
           #:max-n-iterations
           #:max-n-svs
           #:supervise-svd))
