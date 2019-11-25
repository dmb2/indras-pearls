(in-package #:indras-pearls)
(export '(mag
	  moebius-on-point
	  moebius-on-circle
	  circ-to-T))

(defun mag (c)
  (sqrt (+ (expt (realpart c) 2) 
	   (expt (imagpart c) 2))))

(defstruct circle
  (cen #C(0.0 0.0))
  (rad 0.0 :type real))


;; this has bugs in it.. if z is infinite it will error out.  
(defun moebius-on-point (mT z)
  (let ((a (mb-matrix-a mT))
	(b (mb-matrix-b mT))
	(c (mb-matrix-c mT))
	(d (mb-matrix-d mT)))
    (/ (+ (* a z) b) 
       (+ (* c z) d))))

(defun moebius-on-circle (mT C)
  (let* ((P (circle-cen C))
	 (r (circle-rad C))
	 (Q nil)
	 (s nil)
	 (d (mb-matrix-d mT))
	 (c (mb-matrix-c mT))
	 (z nil))
    (when (= c 0)
      (error "Cannot calculate transform when C element of matrix is zero!"))
    (when (= (conjugate (+ (/ d c) P)) 0)
      (error "Cannot calculate transform when d/c + P = 0"))
    (setq z (- P (/ (* r r)
		    (conjugate (+ (/ d c) P)))))
    (setq Q (moebius-on-point mT z)
	  s (mag (- Q (moebius-on-point mT (+ P r)))))
    (make-circle :cen Q :rad s)))

(defun circ-to-T (c1 c2 &optional (k 1) (theta 0))
  "Given two circles `C1' and `C2', produce a moebius transformation
  which maps the interior of `C1' to the exterior of `C2' and vice
  versa. Optionally provide a scaling factor `k' and an angle `theta'
  which maps an arbitrary point in `C1' to infinity outside `C2'. "
  (let* ((r (circle-rad c1))
	 (P (circle-cen c1))
	 (s (circle-rad c2))
	 (Q (circle-cen c2))
	 (exp-itheta (exp (* #C(0 1) theta)))
	 (v (* exp-itheta (sqrt (- (* k k ) 1))))
	 (vb (conjugate v))
	 (u (* k exp-itheta))
	 (ub (conjugate u)))
    (make-mb-matrix :a (+ (* s vb) (* Q u))
		    :b (+ (- (* s P)) (* r ub) (- (* Q P u)) (* r v Q))
		    :c u
		    :d (- (* r v) (* u p)))))

