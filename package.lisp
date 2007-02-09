(cl:defpackage :fsvd
  (:use #:common-lisp)
  (:export #:map-matrix
           #:height-of
           #:width-of
           #:size-of
           #:dense-row-index
           #:dense-column-index
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
