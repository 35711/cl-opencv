(in-package :verrazano)

;; Note that I needed to patch verrazano for this to work.
;; Two patches were required, one from _3b and a nasty and vicious hack I wrote.
;; _3b: http://paste.lisp.org/display/120074#1
;; Mine: http://paste.lisp.org/display/120074#2

(defvar *strip-cv* (cl-ppcre:create-scanner "(^cv|CV|Cv?)"))

(defun go-go-go ()
  (generate-binding
   `(:cffi :package-name :cl-opencv
           :input-files ("/usr/include/opencv2/core/types_c.h"
                         "/usr/include/opencv2/core/core_c.h"
                         "/usr/include/opencv2/imgproc/types_c.h"
                         "/usr/include/opencv2/imgproc/imgproc_c.h"
                         "/usr/include/opencv2/highgui/highgui_c.h")
           :working-directory #P"/home/redline/temp/cl-opencv/"
           :standard-name-transformer-replacements
           ;; drop cv prefix
           (,*cv-scanner* ""))
   :keep-temporary-files nil))
