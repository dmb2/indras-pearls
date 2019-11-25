(in-package #:indras-tests)

(defvar *mT* (indras-pearls::make-mb-matrix
	      :a #C(2 1) :b #C(1 2) :c #C(4 0) :d #C(3 -1)))

(fiasco:deftest moebius-struct () 
  (let ((A (indras-pearls::make-mb-matrix))
	(B (indras-pearls::make-mb-matrix :a #C(1.0 0.0))))
    (is (not (equal A B))) 
    (is (not (equal (indras-pearls::mb-matrix-a A) 
		    (indras-pearls::mb-matrix-a B)))) 
    (is (equal (indras-pearls::mb-matrix-b A) 
	       (indras-pearls::mb-matrix-b B))) 
    (is (equal (indras-pearls::mb-matrix-c A) 
	       (indras-pearls::mb-matrix-c B))) 
    (is (equal (indras-pearls::mb-matrix-d A) 
	       (indras-pearls::mb-matrix-d B)))))

(fiasco:deftest cmag ()
  (is (= 5.0 (mag #C(3 4))))
  (is (= 4.0 (mag 4)))
  (is (= (sqrt 2) (mag #C(1 1)))))

(fiasco:deftest test-circle ()
  (let ((circ (indras-pearls::make-circle :cen #C(1 1) :rad 2.)))
    (is (equal (indras-pearls::circle-cen circ) #C(1 1)))
    (is (equal (indras-pearls::circle-rad circ) 2.))))

(fiasco:deftest test-moebius-on-point ()
  ;; Worked out on paper for *mT*
  (let ((z1-ans #C(9/25 12/25))
	(zi-ans #C(2/3 2/3)))
    (is (= (moebius-on-point *mT* #C(1 0)) z1-ans))	
    (is (= (moebius-on-point *mT* #C(0 1)) zi-ans))))

(fiasco:deftest test-moebius-on-circle ()
  (let* ((circ (indras-pearls::make-circle :cen #C(0 1) :rad 2.))
	 ;; Worked out on paper 
	 (z1-ans (indras-pearls::make-circle 
		  :cen #C(10/23 2/23) 
		  :rad (indras-pearls:mag #C(-63/1495 -491/1495))))
	 (test-ans (moebius-on-circle *mT* circ)))
    (is (= (indras-pearls::circle-rad z1-ans) (indras-pearls::circle-rad test-ans)))
    (is (= (indras-pearls::circle-cen z1-ans) (indras-pearls::circle-cen test-ans)))))
