(in-package #:indras-tests)
(defvar *I* (indras-pearls::make-mb-matrix
	     :a 1 :b 0 :c 0 :d 1))
(defvar *M* (indras-pearls::make-mb-matrix
	     :a 1 :b 1 :c 1 :d 1))
(defvar *M2* (indras-pearls::make-mb-matrix
	      :a #C(0 1) :b 1
	      :c 1       :d #C(0 1)))
(defvar *M3* (indras-pearls::make-mb-matrix
	     :a 2 :b 1 :c 1 :d 1))
(fiasco:deftest m=-test ()
  (is (m= *I* *I*))
  (is (m= *M2* *M2*)))

(fiasco:deftest m*-test ()
  (is (m= (m* *M3* *M*) (make-mb-matrix :a 3 :b 3 :c 2 :c 2))))

(fiasco:deftest det-test ()
  (is (= 0 (det *M*)))
  (is (= -2 (det *M2*))))

(fiasco:deftest tr-test ()
  (is (= 2 (tr *M*)))
  (is (= #C(0 2)) (tr *M2*)))

(fiasco:deftest inv-mat-test ()
  (is (m= (inv-mat *I*) *I*))
  (let ((invM3 (indras-pearls::make-mb-matrix
		:a 1 :b -1 :c -1 :d 2)))
    (is (m= (inv-mat *M3*) invM3))))
