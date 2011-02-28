;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; imgproc.lisp
;;;; OpenCV bindings for SBCL
;;;; Image processing
(in-package :cl-opencv)


;;; Histograms



;;; Image Filtering



;;; Geometric Image Transformations



;;; Miscellaneous Image Transformations

;; Enumeration of threshold types for cvThreshold
(defanonenum
  +thresh-binary+
  +thresh-binary-inv+
  +thresh-trunc+
  +thresh-tozero+
  +thresh-tozero-inv+)

(defcfun ("cvThreshold" %threshold) :double
  (src cv-array)
  (dest cv-array)
  (threshold :double)
  (max-value :double)
  (threshold-type :int))

;; double cvThreshold(const CvArr* src, CvArr* dst, double threshold,
;;                    double maxValue, int thresholdType)
(defun threshold (src dest threshold max-value threshold-type)
  "Applies a fixed-level threshold to array elements. SRC is the
source array and DEST is the target array. THRESHOLD is the threshold
value and MAX-VALUE is the 'on' value for binary
thresholding. THRESHOLD-TYPE is the type of thresholding to be done."
  (%threshold src dest (coerce threshold 'double-float)
              (coerce max-value 'double-float) threshold-type))

;; void cvCvtColor(const CvArr* src, CvArr* dst, int code)
(defcfun ("cvCvtColor" %convert-color) :void
  (src cv-array)
  (dst cv-array)
  (code :int))

(defun cvt-color (src dst code)
  "Converts input array pixels from one color space to another."
  (%convert-color src dst code))



;;; Structural Analysis and Shape Descriptors



;;; Planar Subdivisions



;;; Motion Analysis and Object Tracking



;;; Feature Detection



;;; Object Detection

