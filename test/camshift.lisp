(in-package :cl-opencv-test)

(defun is-rect-nonzero (rect)
  (and (plusp (cv-rect-height rect))
       (plusp (cv-rect-width rect))))
