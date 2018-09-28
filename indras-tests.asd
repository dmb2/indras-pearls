(defsystem "indras-tests"
  :name "Indra's Pearls Tests"
  :serial t
  :depends-on ("indras-pearls"
	       "fiasco")
  :pathname "tests/"
  :components ((:file "package")
	       (:file "moebius"))
  :perform (test-op (o c)
		    (uiop/package:symbol-call "FIASCO" "RUN-TESTS" 'indras-tests)))
