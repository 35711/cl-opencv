(ql:quickload 'fsbv)
(load "/home/redline/projects/cl-opencv/verrazano/verrazano.lisp")
(load "/home/redline/projects/cl-opencv/test/camshift.lisp")

;(print "Maybe you want to run (cv-test:test-tracking)?")

;; cvScalar and rgb don't work at all.
;; For now we try just using the literals via the defctype
;(print (opencv-verrazano::rgb 255.0d0 255.0d0 255.0d0))
;(opencv-verrazano::scalar 255.0d0 255.0d0 255.0d0 0.0d0)

; Hopefully we get through the rectangle call
(opencv-verrazano::test-tracking)
;(opencv-verrazano::show-video)
;(opencv-verrazano::show-video :source "/home/redline/Desktop/cpercival.flv")
