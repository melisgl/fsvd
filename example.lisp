(let* ((m (make-array '(3 5) :element-type 'fixnum
                      :initial-contents '((1 2 3 4 5)
                                          (2 4 6 8 12)
                                          (3 6 9 12 15))))
       (svd (fsvd:svd m :learning-rate 0.01
                      :normalization-factor 0.0
                      :supervisor (fsvd:make-supervisor-function
                                   (make-instance 'fsvd:limiting-supervisor
                                                  :max-n-iterations 20
                                                  :max-n-svs 1)))))
  (format t "~S~%" svd)
  (dotimes (row (fsvd:height-of m))
    (dotimes (column (fsvd:width-of m))
      (format t "~,2F " (fsvd:svd-value svd row column)))
    (terpri)))
