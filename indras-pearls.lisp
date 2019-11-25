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

(defmacro with-canvas (file width height &rest body)
 `(vecto:with-canvas (:width ,width :height ,height)
    (vecto:translate (/ ,width 2) (/ ,height 2))
    ,@body
   (vecto:save-png ,file)))

(defun circles (file)
  (with-canvas file 600 600
    (let* ((r 200)
	   (c1 (make-circle :cen #C(1.16 0.91) :rad 1))
	   (c2 (make-circle :cen #C(-1.16 -0.91) :rad 1))
	   (mT (make-mb-matrix :a #C(1 -.5)
			       :b 0.04
			       :c 0.39
			       :d #C(1 -.5)))
	   (mT2 (inv-mat mT))
	   (black (cl-color-to-list cl-colors:+black+)))
      (loop for i from 1 to 600
	    do
	       (plot-circle c1 black r)
	       (plot-circle c2 black r)
	       (setq c1 (moebius-on-circle mT c1)
		     c2 (moebius-on-circle mT2 c2))))))

(defun schottky-pairs (file)
  (with-canvas file 1000 1000
    (let* ((r 1)
	   (c1 (make-circle :cen #C(-1. -1.) :rad 0.5))
	   (c2 (make-circle :cen #C(1. 1.) :rad 0.5))
	   (mT (circ-to-T c1 c2 0.02 0.001))
	   (inv-mT (inv-mat mT))
	   (black (cl-color-to-list cl-colors:+black+)))
      (vecto:set-rgba-stroke 0 0 0 0.5) 
      (loop for i from 1 to 100
	    do
	       (plot-circle c1 black r)
	       (plot-circle c2 black r)
	       (setq c1 (moebius-on-circle mT c1)
		     c2 (moebius-on-circle inv-mT c2))
	       (vecto:save-png (format nil "~a~3,'0D" file i))))))

(defun extend-words (inv gens word-list)
  (loop
    for g in gens 
    nconc (loop for w in word-list
		unless (= (nth (second w) inv) (position g gens))
		  collect (list (m* (first w) g) (position g gens)))))

(defun schottky-group (file)
  (with-canvas file 1000 1000
    (let* ((r 10)
	   (circ `(,(make-circle :cen #C(-1. -1.) :rad 1)
		    ,(make-circle :cen #C(1. 1.) :rad 1)
		    ,(make-circle :cen #C(-1 1) :rad 0.5)
		    ,(make-circle :cen #C(1 -1) :rad 0.5)))
	   (gens `(,(circ-to-T (nth 0 circ) (nth 1 circ) 1 0.00)
		   nil 
		   ,(circ-to-T (nth 2 circ) (nth 3 circ) 1 0.00)
		   nil))
	   (words nil)
	   (inv '(1 0 3 2)))
      (setf (nth 1 gens) (inv-mat (nth 0 gens))
	    (nth 3 gens) (inv-mat (nth 2 gens)))
      (setq words (extend-words inv gens (mapcar #'list gens '(0 1 2 3))))
      (vecto:set-rgba-stroke 0 0 0 0.5)
      ;; keep these separate from below so that we can control how
      ;; many layers we create, this loop is just for plotting the
      ;; initial circles
      (loop for i from 0 to 3
	    do
	       (plot-circle (nth i circ) cl-colors:+black+ r))
      (loop for i from 0 to 3
	    do
	       (setq words (extend-words inv gens words)))
      (mapc (lambda (word)
	      (mapc (lambda (c)
		      (plot-circle c cl-colors:+black+ r))
		    (mapcar (lambda (c)
			      (moebius-on-circle (first word) c))
			    circ)))
	    words))))


(defun random-circles (file)
  (with-canvas file 1950 1080
    (let* ((r 10)
	   (c nil)
	   (colors (gen-colors cl-colors:+black+ cl-colors:+blue+ 60)))
      (loop for i from 1 to 40 
	    do
	       (loop for j from 1 to 15
		     do
			(setq c (make-circle :cen (complex (* 2 (- i 20)) (* 2 (- j 7))) :rad 1)) 
			(plot-circle c '(0 0 0) r))
	    ))))
