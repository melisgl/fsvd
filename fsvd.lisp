(in-package :fsvd)

;;; Possibly sparse matrix interface

(defgeneric height-of (matrix &key densep)
  (:documentation "Return the number of rows of MATRIX."))

(defgeneric width-of (matrix &key densep)
  (:documentation "Return the number of columns of MATRIX."))

(defgeneric size-of (matrix)
  (:documentation "Return the number known cells in MATRIX. This is an
upper limit for dense indices produced by MAP-MATRIX."))

(defgeneric map-row-densely (matrix row)
  (:documentation "Number those rows that are not empty from 0. Return
NIL for empty rows."))

(defgeneric map-column-densely (matrix column)
  (:documentation "Number those columns that are not empty from 0.
Return NIL for empty columns."))

(defgeneric map-matrix (function matrix)
  (:documentation "Call FUNCTION for each non-missing cell of MATRIX.
FUNCTION is of four parameters: ROW, COLUMN, VALUE and DENSE-INDEX."))

(defmacro do-matrix (((row column value dense-index) matrix)
                     &body body)
  "A simple, inefficient implementation of the macro interface to
iterate over MATRIX."
  `(map-matrix (lambda (,row ,column ,value ,dense-index)
                 ,@body)
    ,matrix))

(defgeneric do-matrix-macro-name (matrix)
  (:method ((matrix t))
    'do-matrix)
  (:documentation "Return the name of the macro that provides a
hopefully efficient way to iterate over MATRIX. See DO-MATRIX for an
example."))

;;; Vector utilities

(deftype single-float-vector () '(simple-array single-float (*)))
(deftype 2d-single-float-array () '(simple-array single-float (* *)))

(defun make-v (length)
  (let ((r (make-array length :element-type 'single-float)))
    (loop for i below length do
          (setf (aref r i) 0.1))
    r))

;;; Reading and writing float arrays

(defun sync->fd (fd-stream)
  (force-output fd-stream)
  (let ((fd (sb-impl::fd-stream-fd fd-stream)))
    (sb-unix:unix-lseek fd (file-position fd-stream) sb-unix:l_set)))

(defun sync<-fd (fd-stream)
  (let ((fd (sb-impl::fd-stream-fd fd-stream)))
    (file-position fd-stream
                   (sb-unix:unix-lseek fd 0 sb-unix:l_incr))))

(defun write-float-array (array fd-stream)
  (declare (type (simple-array single-float (*)) array))
  (sync->fd fd-stream)
  (let ((fd (sb-impl::fd-stream-fd fd-stream)))
    (sb-unix:unix-write fd
                        (sb-sys:vector-sap array)
                        0
                        (* 4 (length array))))
  (sync<-fd fd-stream))

(defun read-float-array (array fd-stream)
  (declare (type (simple-array single-float (*)) array))
  (sync->fd fd-stream)
  (let* ((l (* 4 (length array)))
         (l2 (sb-unix:unix-read (sb-impl::fd-stream-fd fd-stream)
                                (sb-sys:vector-sap array)
                                l)))
    (sync<-fd fd-stream)
    (unless (= l l2)
      (error "Read only ~S bytes out of ~S~%" l2 l))))

(defun write-float (float stream)
  (declare (type single-float float))
  (let ((v (make-array 1 :element-type 'single-float
                       :initial-contents (list float))))
    (write-byte (sb-sys:sap-ref-32 (sb-sys:vector-sap v) 0) stream)))

;;; Singular value decomposition

(defstruct sv
  "SV is pair of vectors."
  (left nil :type (simple-array single-float *))
  (right nil :type (simple-array single-float *)))

(defun create-sv (height width)
  (make-sv :left (make-v height) :right (make-v width)))

(deftype svd ()
  "SVD consists of n SVs - pairs of left and right vector - of the
same sizes. The matrix SVD represents in this form is obtained by
summing pairwise the outer products of these vectors."
  `(simple-array sv (*)))

(defun make-svd ()
  "Create an empty SVD."
  (make-array 0 :element-type 'sv :initial-element (create-sv 0 0)))

(defun compact-sv (sv matrix)
  "Take a SV that uses sparse indices of matrix and turn it into one
that uses dense indices."
  (flet ((compact (target source map)
           (loop for i below (length source) do
                 (let ((dense-i (funcall map matrix i)))
                   (when dense-i
                     (setf (aref target dense-i) (aref source i)))))))
    (let ((new-sv (create-sv (height-of matrix :densep t)
                             (width-of matrix :densep t))))
      (compact (sv-left new-sv) (sv-left sv) #'map-row-densely)
      (compact (sv-right new-sv) (sv-right sv) #'map-column-densely)
      new-sv)))

(defun save-svd (svd filename)
  "Write the content of SVD to FILENAME in a reasonably compact form."
  (with-open-file (stream filename :direction :output
                   :if-does-not-exist :create
                   :if-exists :supersede)
    (let ((sv (if (zerop (length svd))
                  (create-sv 0 0)
                  (aref svd 0))))
      (format stream "~S~%" (list :length (length svd)
                                  :left-size (length (sv-left sv))
                                  :right-size (length (sv-right sv)))))
    (loop for sv across svd do
          (write-float-array (sv-left sv) stream)
          (write-float-array (sv-right sv) stream)))
  (values))

(defun load-svd (filename)
  "Return the SVD loaded from FILENAME."
  (with-open-file (stream filename)
    (destructuring-bind (&key length left-size right-size)
        (read-from-string (read-line stream))
      (let ((svd (make-array length :element-type 'sv)))
        (loop for i below length do
              (let ((sv (create-sv left-size right-size)))
                (read-float-array (sv-left sv) stream)
                (read-float-array (sv-right sv) stream)
                (setf (aref svd i) sv)))
        svd))))

(defun svd-value (svd row column &key (base-value 0.0) (clip #'identity))
  "Return the value of the matrix represented by SVD at ROW and
COLUMN. Start the summation from BASE-VALUE and CLIP the current sum
to some valid range if any after every pass."
  (let* ((clip (coerce clip 'function))
         (sum (funcall clip base-value)))
    (locally
        (declare (type svd svd)
                 (type single-float sum))
      (loop for sv across svd do
            (setf sum (funcall clip (+ sum
                                       (* (aref (sv-left sv) row)
                                          (aref (sv-right sv) column)))))))
    sum))

(defun append-to-svd (svd sv)
  (let ((r (make-array (1+ (length svd)))))
    (replace r svd)
    (setf (aref r (length svd)) sv)
    r))

;;; This is the performance critical loop. Compile it for each run.
(defmacro train/epoch (&key matrix approximation
                       learning-rate normalization-factor
                       clip do-matrix)
  `(lambda (left right)
     (declare (type single-float-vector left right)
              (inline ,clip)
              (optimize (speed 3)))
     (,do-matrix ((row column value index) ,matrix)
       (let* ((l (aref left row))
              (r (aref right column))
              (err (- value (,clip (+ (aref ,approximation index)
                                      (* l r))))))
         (setf (aref right column) (+ r
                                      (* ,learning-rate
                                         (- (* err l)
                                            (* ,normalization-factor r)))))
         (setf (aref left row) (+ l
                                  (* ,learning-rate
                                     (- (* err r)
                                        (* ,normalization-factor l)))))))))

(defun svd-1 (matrix &key trainer svd supervisor)
  (declare (type svd svd))
  ;; We work with sparse indices to avoid having to map indices and
  ;; compact SV then necessary.
  (let* ((sv (create-sv (height-of matrix :densep nil)
                        (width-of matrix :densep nil)))
         (left (sv-left sv))
         (right (sv-right sv)))
    (loop for i upfrom 0 do
          (funcall trainer left right)
          (unless (funcall supervisor
                           (append-to-svd svd (compact-sv sv matrix))
                           i)
            (return)))
    (compact-sv sv matrix)))

(defun svd (matrix &key (svd (make-svd)) (base-approximator (constantly 0.0))
            (learning-rate 0.001) (normalization-factor 0.02)
            supervisor (clip 'identity))
  "Approximate the single-float MATRIX with a quasi singular value
decomposition. Each SV of an SVD consists of a left and a right
vector. The sum of the outer products of the left and right vectors of
its consituent SVs is the approximation provided by an SVD. This SVD
is quasi because while the rank of the approximation is N, the
singular values are not explicit.

BASE-APPROXIMATOR is a function of row and column. Its values are
effectively subtracted from those of MATRIX. Don't forget to supply
the same BASE-APPROXIMATOR to MAKE-SVD-APPROXIMATOR and
SVD-VALUE.

CLIP is a symbol that is fbound to a function that takes a single
single-float and returns it clamped into some valid range or leaves it
alone.

To make training fast, a new trainer function is compiled for each SVD
call using CLIP and DO-MATRIX-MACRO-NAME for MATRIX.

LEARNING-RATE controls the how much weights are drawn towards to
optimum at each step while the Tikhonov NORMALIZATION-FACTOR penalizes
large weights.

SUPERVISOR is a function of two arguments. The first argument is
always the current SVD. The second value is NIL when it is called just
before a new SV is added to the SVD, and when the supervisor function
is called while a single SV is being tuned it is a number that denotes
the current iteration that was finished. If SUPERVISOR returns NIL the
tuning of the current SV or the whole process in terminated depending
on the calling context that is evident in its second argument."
  (let* ((approximation (make-array (size-of matrix)
                                    :element-type 'single-float))
         (trainer (compile nil
                           (eval
                            (list 'train/epoch :matrix matrix
                                  :approximation approximation
                                  :learning-rate learning-rate
                                  :normalization-factor normalization-factor
                                  :clip clip
                                  :do-matrix (do-matrix-macro-name matrix)))))
         (clip (coerce clip 'function)))
    (map-matrix
     (lambda (row column value i)
       (declare (ignore value))
       (setf (aref approximation i)
             (funcall clip (funcall base-approximator row column))))
     matrix)
    (flet ((supervise (svd i)
             (when supervisor
               (funcall supervisor svd i :base-approximator base-approximator
                        :clip clip :matrix matrix
                        :approximation approximation)))
           (add (sv)
             (let ((left (sv-left sv))
                   (right (sv-right sv)))
               (declare (type single-float-vector approximation))
               (map-matrix
                (lambda (row column value i)
                  (declare (ignore value))
                  (let ((row (map-row-densely matrix row))
                        (column (map-column-densely matrix column)))
                    (setf (aref approximation i)
                          (funcall clip
                                   (+ (aref approximation i)
                                      (* (aref left row)
                                         (aref right column)))))))
                matrix))))
      (loop for sv across svd do
            (add sv))
      (loop for n upfrom (length svd) do
            (unless (supervise svd nil)
              (return svd))
            (let ((sv (svd-1 matrix :trainer trainer :svd svd
                             :supervisor #'supervise)))
              (add sv)
              (setf svd (append-to-svd svd sv)))))))

(defun make-svd-approximator (svd &key matrix (max-n (length svd))
                              (base-approximator (constantly 0))
                              (clip #'identity))
  "Return a function of (ROW COLUMN) parameters that approximates
MATRIX by SVD. The BASE-VALUE for SVD-VALUE is produced by
BASE-APPROXIMATOR for the given coordinates, while CLIP is simply
passed on. The returned function automatically translates to dense
coordinates to query the SVD."
  (let ((svd (subseq svd 0 max-n)))
    (lambda (row column)
      (let ((base-value (funcall base-approximator row column))
            (row (map-row-densely matrix row))
            (column (map-column-densely matrix column)))
        (svd-value svd row column :base-value base-value :clip clip)))))

;;; Utility class

(defclass limiting-supervisor ()
  ((svd-in-progress :initform (make-svd) :accessor svd-in-progress)
   (max-n-iterations :initform nil :initarg :max-n-iterations
                     :accessor max-n-iterations)
   (max-n-svs :initform nil :initarg :max-n-svs :accessor max-n-svs)
   (subsupervisor :initform nil :initarg :subsupervisor
                  :accessor subsupervisor)))

(defgeneric supervise-svd (supervisor svd iteration &key base-approximator clip)
  (:method ((supervisor limiting-supervisor) svd iteration &key
            base-approximator clip matrix approximation)
    (with-slots (svd-in-progress max-n-iterations max-n-svs subsupervisor)
        supervisor
      (setf svd-in-progress svd)
      (and (or (null max-n-svs)
               (<= (+ (length svd) (if iteration 0 1)) max-n-svs))
           (or (null iteration) (null max-n-iterations)
               (< iteration max-n-iterations))
           (or (null subsupervisor)
               (funcall subsupervisor svd iteration
                        :base-approximator base-approximator
                        :clip clip :matrix matrix
                        :approximation approximation))))))

(defun make-supervisor-function (supervisor)
  (lambda (&rest args)
    (apply #'supervise-svd supervisor args)))

;;; Simplistic implementation of the FSVD matrix interface for 2D arrays

(defmethod height-of ((array array) &key densep)
  (declare (ignore densep))
  (array-dimension array 0))

(defmethod width-of ((array array) &key densep)
  (declare (ignore densep))
  (array-dimension array 1))

(defmethod size-of ((array array))
  (array-total-size array))

(defmethod map-row-densely ((array array) row)
  (declare (ignore array))
  row)

(defmethod map-column-densely ((array array) column)
  (declare (ignore array))
  column)

(defmethod map-matrix (function (array array))
  (loop for row below (height-of array) do
        (loop for column below (width-of array) do
              (when (aref array row column)
                (funcall function row column (aref array row column)
                         (array-row-major-index array row column))))))

(defmacro do-array-matrix (((row column value dense-index) matrix)
                           &body body)
  (let ((width (gensym))
        (%matrix (gensym)))
    `(let* ((,%matrix ,matrix)
            (,dense-index 0)
            (,width (the fixnum (width-of ,%matrix))))
       (dotimes (,row (the fixnum (height-of ,%matrix)))
         (dotimes (,column ,width)
           (let ((,value (aref ,%matrix ,row ,column)))
             (when ,value
               ,@body)
             (incf ,dense-index)))))))

(defmethod do-matrix-macro-name ((array array))
  'do-array-matrix)
