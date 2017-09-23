;;;; indras-pearls.lisp

(in-package #:indras-pearls)

;;; "indras-pearls" goes here. Hacks and glory await!

(defun mag (c)
  (sqrt (+ (expt (realpart c) 2) 
	   (expt (imagpart c) 2))))

(defun gradient-bilinear-example (file)
  (vecto:with-canvas (:width 200 :height 50)
    (vecto:set-gradient-fill 25 0
                       1 0 0 1
                       175 0
                       1 0 0 0
                       :domain-function 'vecto:bilinear-domain)
    (vecto:rectangle 0 0 200 50)
    (vecto:fill-path)
    (vecto:save-png file)))

(defun hexagon (sf x0 y0)
  (let ((X '(3/2 0 -3/2 -3/2 0 3/2 3/2))
	(Y (mapcar (lambda (x) (* (sqrt 3) x)) '(1/2 1 1/2 -1/2 -1 -1/2 1/2 1/2))))
    (vecto:move-to (+ (* sf (first X)) x0) 
		   (+ (* sf (first Y)) y0))
    (mapc (lambda (x y)
	    (vecto:line-to (+ (* sf x) x0) 
			   (+ (* sf y) y0))) 
	  (rest X) (rest Y)))
        (vecto:stroke))

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

(defun circle (x y r)
  (vecto:centered-circle-path x y r)
  (vecto:stroke))

(defun circles (file)
  (let ((w 400)
	(h 400)
	(r 100))
    (vecto:with-canvas (:width w :height h)
      (vecto:set-rgb-stroke 0 0 0)
      (circle (/ w 2) (/ h 2) r)
      (vecto:save-png file))))


