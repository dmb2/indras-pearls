;;;; indras-pearls.lisp

(in-package #:indras-pearls)

;;; "indras-pearls" goes here. Hacks and glory await!

(defun tiled-hexagons (file)
  (let ((w 400)
	(h 400)
	(sf 10))
    (vecto:with-canvas (:width w :height h)
      (vecto:set-rgb-stroke 0 0 0)
      (loop for k from -10 to 10
	 append (loop for l from -10 to 10
		   do 
		     (hexagon sf (+ (* k 3 sf) (* 1.5 sf l) (/ w 2)) 
			      (+ (* sf l 1.5 (sqrt 3))  (/ h 2)))))
      (vecto:save-png file))))

(defun circles (file)
  (let ((w 400)
	(h 400)
	(r 400)
	(c1 (make-circle :cen #C(1.16 0.91) :rad 1))
	(c2 (make-circle :cen #C(-1.16 -0.91) :rad 1))
	(mT (make-moebius-matrix :a #C(1 -.5)
				 :b 0.04
				 :c 0.39
				 :d #C(1 -.5)))
	(mT2 (make-moebius-matrix :a #C(1 -.5)
				 :b -0.04
				 :c -0.39
				 :d #C(1 -.5)))
	
	(coffee (cl-colors:as-rgb "c0ffee"))
	(black (cl-colors:as-rgb "000000")))
    (vecto:with-canvas (:width w :height h)
      (vecto:translate (/ w 2) (/ h 2))
      (vecto:set-rgb-stroke 0 0 0)
      (loop for i from 1 to 600
	 do
	   (plot-circle c1 coffee r)
	   (plot-circle c2 black r)
	   (setq c1 (moebius-on-circle mT c1)
		 c2 (moebius-on-circle mT2 c2)))
      (vecto:save-png file))))
