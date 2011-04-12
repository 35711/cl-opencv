(ql:quickload '(cl-gtk2-gtk fsbv))
(load "/home/redline/projects/cl-opencv/verrazano/verrazano.lisp")
(load "/home/redline/projects/cl-opencv/test/camshift.lisp")

;(opencv-verrazano::test-tracking-gui)
(opencv-verrazano::test-tracking)
;(opencv-verrazano::show-video)
;(opencv-verrazano::show-video :source "/home/redline/Desktop/cpercival.flv")
