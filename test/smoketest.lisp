(ql:quickload 'fsbv)
(load "/home/redline/projects/cl-opencv/verrazano/verrazano.lisp")
(load "/home/redline/projects/cl-opencv/test/camshift.lisp")

;(print "Maybe you want to run (cv-test:test-tracking)?")

(opencv-verrazano::test-tracking)
;(opencv-verrazano::show-video)
;(opencv-verrazano::show-video :source "/home/redline/Desktop/cpercival.flv")
