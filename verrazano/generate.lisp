(in-package :verrazano)

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
