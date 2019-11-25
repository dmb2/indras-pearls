(in-package #:indras-pearls)

(defstruct mb-matrix
  (a #C(0.0 0.0))
  (b #C(0.0 0.0))
  (c #C(0.0 0.0))
  (d #C(0.0 0.0)))

(export '(tr
	  det
	  m=
	  m*
	  inv-mat))

(defun m= (M1 M2)
  (and (= (mb-matrix-a M1) (mb-matrix-a M2))
       (= (mb-matrix-b M1) (mb-matrix-b M2))
       (= (mb-matrix-c M1) (mb-matrix-c M2))
       (= (mb-matrix-d M1) (mb-matrix-d M2))))

(defun m* (M1 M2)
  (let ((a1 (mb-matrix-a M1))
	(a2 (mb-matrix-a M2))
	(b1 (mb-matrix-b M1))	
	(b2 (mb-matrix-b M2))
	(c1 (mb-matrix-c M1))	
	(c2 (mb-matrix-c M2))
	(d1 (mb-matrix-d M1))	
	(d2 (mb-matrix-d M2)))
    (make-mb-matrix :a (+ (* a1 a2) (* b1 c2))
		    :b (+ (* a1 b2) (* b1 d2))
		    :c (+ (* c1 a2) (* d1 c2))
		    :d (+ (* c1 b2) (* d1 d2)))))

(defun tr (M)
  (+ (mb-matrix-a M) (mb-matrix-d M)))

(defun det (mat)
  (- (* (mb-matrix-a mat)
	(mb-matrix-d mat))
     (* (mb-matrix-b mat)
	(mb-matrix-c mat))))

(defun inv-mat (M)
  (let ((det-M (det M)))
    (when (= det-M 0)
      (error "Matrix not invertible, determinant is 0"))
    (make-mb-matrix :a (/ (mb-matrix-d M) det-M)
		    :b (/ (- (mb-matrix-b M)) det-M)
		    :c (/ (- (mb-matrix-c M)) det-M)
		    :d (/ (mb-matrix-a M) det-M))))
