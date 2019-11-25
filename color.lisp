(in-package :indras-pearls)

(defstruct hcl-color
  (hue 0 :type real)
  (chroma 0 :type real)
  (luminance 0 :type real))

(defun interpolate-color (color1 color2 z)
  " `color1' and `color2' are lists of three numbers in order to keep
  things general. The interpolation parameter `z' is in the range
  [0,1]."
  (when (or (< z 0) (> z 1)))
  (map 'list (lambda (x y) (+ (* z x) (* (- 1 z) y))) color1 color2))

(defun rgb-to-mhvc (r g b)
  (multiple-value-bind (x y z)
      (dufy:rgb-to-xyz r g b)
    (dufy:xyz-to-mhvc x y z)))

(defun mhvc-to-rgb (color)
  (let ((h (nth 0 color))
	(v (nth 1 color))
	(c (nth 2 color)))
    (mapcar (lambda (x)
	      (alexandria:clamp x 0 1))
	    (multiple-value-list
	     (dufy:mhvc-to-lrgb h v c)))))

(defun cl-color-to-rgb (color)
  (values (cl-colors:rgb-red color)
	  (cl-colors:rgb-green color)
	  (cl-colors:rgb-blue color)))

(defun cl-color-to-list (color)
  (multiple-value-list (cl-color-to-rgb color)))

(defun gen-colors (color1 color2 n)
  "Given `color1' and `color2' (of type cl-colors:color) as the
  starting and ending colors, and `n' as the number of colors
  requested, this routine returns a list of colors of type
  cl-colors:color."
  (let ((col1 (multiple-value-list (rgb-to-mhvc (cl-colors:rgb-red color1)
						(cl-colors:rgb-green color1)
						(cl-colors:rgb-blue color1))))
	(col2 (multiple-value-list (rgb-to-mhvc (cl-colors:rgb-red color2)
						(cl-colors:rgb-green color2)
						(cl-colors:rgb-blue color2)))))
    (loop for i from 0 to n collect
			    (mhvc-to-rgb
			     (interpolate-color col1
						col2
						(/ i n))))))

