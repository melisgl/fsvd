(defun test-svd (n-svs n-iterations learning-rate contents)
  (let* ((m (make-array (list (length contents)
                              (length (first contents)))
                        ;; Declare the type to help us silence
                        ;; compilation warnings coming from the
                        ;; compilation of inner training loop.
                        :element-type (list* 'member (apply #'append contents))
                        :initial-contents contents))
         (svd (fsvd:svd m :learning-rate learning-rate
                        :normalization-factor 0.0
                        :supervisor
                        (fsvd:make-supervisor-function
                         (make-instance 'fsvd:limiting-supervisor
                                        :max-n-iterations n-iterations
                                        :max-n-svs n-svs)))))
    (format t "~S~%" svd)
    (dotimes (row (fsvd:height-of m))
      (dotimes (column (fsvd:width-of m))
        (format t "~,2F " (fsvd:svd-value svd row column)))
      (terpri))))

(test-svd 1 100 0.01 '((1 2 3 4 5)
                       (2 4 6 8 10)
                       (3 6 9 12 15)))

(test-svd 2 1000 0.01 '((1 2 3 4 5)
                        (2 3 4 5 10)
                        (3 6 9 12 15)))

;;; The same two with missing values.

(test-svd 1 100 0.01 '((1 2 3 nil 5)
                       (2 4 6 8 10)
                       (3 nil nil 12 15)))

(test-svd 2 1000 0.01 '((1 2 3 4 nil)
                        (2 3 4 5 10)
                        (nil 6 nil 12 15)))
