;;;; indras-pearls.asd

(asdf:defsystem #:indras-pearls
  :description "Code for making the fractals found in \"Indra's Pearls: The Vision of Felix Klein\" "
  :author "David Bjergaard <dbjergaard@gmail.com>"
  :license "GPLv3"
  :depends-on (#:vecto
	       #:dufy
	       #:alexandria
	       #:cl-colors)
  :serial t
  :components ((:file "package")
	       (:file "linear-algebra")
	       (:file "color")
	       (:file "moebius")
	       (:file "draw")
               (:file "indras-pearls")))
