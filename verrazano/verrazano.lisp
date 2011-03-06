(defpackage :opencv-verrazano
  (:use :cl :cffi))

(in-package :opencv-verrazano)


;;;;; NOTES


;;; We actually can handle structs by value if needed with
;;; FSBV (Foreign Structures By Value), available through quicklisp.
;;; It adds a requirement on libffi and fails to build on my system
;;; because it can't find the ffi.h file to grovel, so libffi-unix.lisp
;;; may need patching. TODO?

;; Takes at least one passed-by-value struct
; cvCreateImage create-image
; cvGetSubRect get-sub-rect
; cvRectangle rectangle
; cvEllipseBox ellipse-box
; cvSetMouseCallback set-mouse-callback
; cvCamShift %camshift

;; Returns a struct (not a pointer to struct)
; cvGetSize get-size


;;;;; load-libs


(define-foreign-library opencv-core
  (:darwin (:or "libopencv_core.2.2.0.dylib" "libopencv_core.dylib"))
  (:unix (:or "libcore.so.2.1.0" "libcore.so" "libopencv_core.so"))
  (t (:default "libcore")))

(define-foreign-library opencv-imgproc
  (:darwin (:or "libopencv_imgproc.2.2.0.dylib" "libopencv_imgproc.dylib"))
  (:unix (:or "libimgproc.so.2.1.0" "libimgproc.so" "libopencv_imgproc.so"))
  (t (:default "libimgproc")))

(define-foreign-library opencv-highgui
  (:darwin (:or "libopencv_highgui.2.2.0.dylib" "libopencv_highgui.dylib"))
  (:unix (:or "libhighgui.so.2.1.0" "libhighgui.so" "libopencv_highgui.so"))
  (t (:default "libhighgui")))

(define-foreign-library opencv-video
  (:darwin (:or "libopencv_video.2.2.0.dylib" "libopencv_video.dylib"))
  (:unix (:or "libvideo.so.2.1.0" "libvideo.so" "libopencv_video.so"))
  (t (:default "libvideo")))

(cond ((member :darwin *features*)
       (pushnew #P"/opt/local/lib/" cffi:*foreign-library-directories*))
      ((member :linux *features*)
       (pushnew #P"/usr/lib/" cffi:*foreign-library-directories*)))
(use-foreign-library opencv-core)
(use-foreign-library opencv-imgproc)
(use-foreign-library opencv-highgui)
(use-foreign-library opencv-video)

;;;;; helpers

(defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
   `(progn ,@(loop for value in enums
                for index = 0 then (1+ index)
                when (listp value) do
                  (setf index (second value)
                        value (first value))
                collect `(defconstant ,value ,index))))

(defun vtable-lookup (pobj indx coff)
  (let ((vptr (mem-ref pobj :pointer coff)))
    (mem-aref vptr :pointer (- indx 2))))

(defmacro virtual-funcall (pobj indx coff &body body)
  `(foreign-funcall-pointer (vtable-lookup ,pobj ,indx ,coff) nil
                            ,@body))


;;;;; types


(defcstruct size
  (width :int)
  (height :int))

(defcstruct rect
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defctype mouse-callback :pointer)

(defcstruct point
  (x :int)
  (y :int))

(defcstruct scalar
  (val :double :count 4))

(defcstruct point-2d-32f
  (x :float)
  (y :float))

(defcstruct size-2d-32f
  (width :float)
  (height :float))

(defcstruct box-2d
  (center point-2d-32f)
  (size size-2d-32f)
  (angle :float))

(defcstruct term-criteria
  (type :int)
  (max-iter :int)
  (epsilon :double))


;;;;; enums


(defanonenum
  (+hist-array+ 0)
  (+termcrit-iter+ 1)
  (+termcrit-eps+ 2)
  (+aa+ 16)
  (+event-lbuttondown+ 1)
  (+event-lbuttonup+ 4)
  (+bgr-to-hsv+ 40)
  (+hsv-to-bgr+ 54)
  (+window-autosize+ 1))

;; uh-oh?
;; TODO: Should construct a function rgb which returns a scalar
;; object filled with the appropriate values.
;; #define CV_RGB( r, g, b )  cvScalar( (b), (g), (r), 0 )


;;;;; core/


;; Returns width and height of array in elements
;; CvSize cvGetSize(const CvArr* arr)
(defcfun ("cvGetSize" get-size) size
  (arr :pointer))

;; Copies source array to destination array
;; void cvCopy(const CvArr* src, CvArr* dst,
;;             const CvArr* mask CV_DEFAULT(NULL))
(defcfun ("cvCopy" copy) :void
  (src :pointer)
  (dst :pointer)
  (mask :pointer))

;; Retrieves number of an array dimensions and
;; optionally sizes of the dimensions
;; int cvGetDims(const CvArr* arr, int* sizes CV_DEFAULT(NULL))
(defcfun ("cvGetDims" get-dims) :int
  (arr :pointer)
  (sizes (:pointer :int)))

;; Splits a multi-channel array into the set of single-channel
;; arrays or extracts particular [color] plane
;; void cvSplit(const CvArr* src, CvArr* dst0, CvArr* dst1,
;;              CvArr* dst2, CvArr* dst3)
(defcfun ("cvSplit" split) :void
  (src :pointer)
  (dst-0 :pointer)
  (dst-1 :pointer)
  (dst-2 :pointer)
  (dst-3 :pointer))

;; Creates IPL image (header and data)
;; IplImage* cvCreateImage(CvSize size, int depth, int channels)
(defcfun ("cvCreateImage" create-image) :pointer
  (size size)
  (depth :int)
  (channels :int))

;; Makes a new matrix from <rect> subrectangle of input array.
;; No data is copied
;; CvMat* cvGetSubRect(const CvArr* arr, CvMat* submat, CvRect rect)
(defcfun ("cvGetSubRect" get-sub-rect) :pointer
  (arr :pointer)
  (submat :pointer)
  (rect rect))

;; Finds global minimum, maximum and their positions
;; void cvMinMaxLoc(const CvArr* arr, double* min_val, double* max_val,
;;                  CvPoint* min_loc CV_DEFAULT(NULL),
;;                  CvPoint* max_loc CV_DEFAULT(NULL),
;;                  const CvArr* mask CV_DEFAULT(NULL))
(defcfun ("cvMinMaxLoc" min-max-loc) :void
  (arr :pointer)
  (min-val (:pointer :double))
  (max-val (:pointer :double))
  (min-loc :pointer)
  (max-loc :pointer)
  (mask :pointer))

;; Performs linear transformation on every source array element:
;;    dst(x,y,c) = scale*src(x,y,c)+shift.
;; Arbitrary combination of input and output array depths are
;; allowed (number of channels must be the same), thus the
;; function can be used for type conversion
;; void cvConvertScale(const CvArr* src, CvArr* dst,
;;                     double scale CV_DEFAULT(1),
;;                     double shift CV_DEFAULT(0))
(defcfun ("cvConvertScale" convert-scale) :void
  (src :pointer)
  (dst :pointer)
  (scale :double)
  (shift :double))

;; Creates an exact copy of the input matrix (except, may be,
;; step value)
;; CvMat* cvCloneMat(const CvMat* mat)
(defcfun ("cvCloneMat" clone-mat) :pointer
  (mat :pointer))

;; Creates an exact copy of the input matrix (except, may be,
;; step value)
;; CvMat* cvCloneMat(const CvMat* mat)
(defcfun ("cvCloneMatND" clone-mat-nd) :pointer
  (mat :pointer))

;; Draws a rectangle given two opposite corners of the rectangle
;; (pt1 & pt2), if thickness<0 (e.g. thickness == CV_FILLED),
;; the filled box is drawn
;; void cvRectangle(CvArr* img, CvPoint pt1, CvPoint pt2,
;;                  CvScalar color, int thickness CV_DEFAULT(1),
;;                  int line_type CV_DEFAULT(8),
;;                  int shift CV_DEFAULT(0))
(defcfun ("cvRectangle" rectangle) :void
  (img :pointer)
  (pt-1 point)
  (pt-2 point)
  (color scalar)
  (thickness :int)
  (line-type :int)
  (shift :int))

;; TODO: This is throwing "undefined alien" style-warnings
;; because it's inlined. Fix it.
;; void cvEllipseBox(CvArr* img, CvBox2D box, CvScalar color,
;;                  int thickness CV_DEFAULT(1),
;;                  int line_type CV_DEFAULT(8),
;;                  int shift CV_DEFAULT(0))
(defcfun ("cvEllipseBox" ellipse-box) :void
  (img :pointer)
  (box box-2d)
  (color scalar)
  (thickness :int)
  (line-type :int)
  (shift :int))

;; Releases IPL image header and data
;; void cvReleaseImage(IplImage** image)
(defcfun ("cvReleaseImage" release-image) :void
  (image :pointer))


;;;;; highgui/


;; create window
;; int cvNamedWindow(const char* name, int flags CV_DEFAULT(CV_WINDOW_AUTOSIZE))
(defcfun ("cvNamedWindow" named-window) :int
  (name :string)
  (flags :int))

;; TODO: In testing the window frame grays out but stays around. What gives?
;; destroy window and all the trackers associated with it
;; void cvDestroyWindow(const char* name)
(defcfun ("cvDestroyWindow" destroy-window) :void
  (name :string))

;; display image within window (highgui windows remember their content)
;; void cvShowImage(const char* name, const CvArr* image)
(defcfun ("cvShowImage" show-image) :void
  (name :string)
  (image :pointer))

;; wait for key event infinitely (delay<=0) or for "delay" milliseconds
;; int cvWaitKey(int delay CV_DEFAULT(0))
(defcfun ("cvWaitKey" wait-key) :int
  (delay :int))

;; assign callback for mouse events
;; callbacks should take 4 ints (event, x, y, flags) and a void*
;; void cvSetMouseCallback(const char* window_name,
;;                         CvMouseCallback on_mouse,
;;                         void* param CV_DEFAULT(NULL))
(defcfun ("cvSetMouseCallback" set-mouse-callback) :void
  (window-name :string)
  (on-mouse mouse-callback)
  (param (:pointer :void)))

;; Just a combination of cvGrabFrame and cvRetrieveFrame
;; !!!DO NOT RELEASE or MODIFY the retrieved frame!!!
;; IplImage* cvQueryFrame(CvCapture* capture)
(defcfun ("cvQueryFrame" query-frame) :pointer
  (capture :pointer))

;; start capturing frames from video file
;; CvCapture* cvCreateFileCapture(const char* filename)
(defcfun ("cvCreateFileCapture" create-file-capture) :pointer
  (filename :string))

;; start capturing frames from camera:
;; index = camera_index + domain_offset (CV_CAP_*)
;; CvCapture* cvCreateCameraCapture(int index)
(defcfun ("cvCreateCameraCapture" create-camera-capture) :pointer
  (index :int))

;; stop capturing/reading and free resources
;; void cvReleaseCapture(CvCapture** capture)
(defcfun ("cvReleaseCapture" release-capture) :void
  (capture :pointer))

(defmacro with-video ((name source) &body body)
  "Bind NAME to SOURCE and execute BODY ensuring that any
resources used are released."
  (let ((capture-ptr (gensym)))
    `(let ((,name ,source))
       (unwind-protect (progn ,@body)
         ;; release-capture takes a CvCapture** whereas
         ;; name is a CvCapture* necessitating this tomfoolery
         (with-foreign-object (,capture-ptr :pointer)
           (setf (mem-ref ,capture-ptr :pointer) ,name)
           (release-capture ,capture-ptr))))))

(defmacro with-window ((name size) &body body)
  (let ((window (gensym)))
    `(let ((,window (named-window ,name ,size)))
       (declare (ignore ,window))
       (unwind-protect (progn ,@body)
         (destroy-window ,name)))))


;;;;; imgproc/


;; Converts input array pixels from one color space to another
;; void cvCvtColor(const CvArr* src, CvArr* dst, int code)
(defcfun ("cvCvtColor" cvt-color) :void
  (src :pointer)
  (dst :pointer)
  (code :int))

;; Finds indices and values of minimum and maximum histogram bins
;; void cvGetMinMaxHistValue(const CvHistogram* hist,
;;                           float* min_value, float* max_value,
;;                           int* min_idx CV_DEFAULT(NULL),
;;                           int* max_idx CV_DEFAULT(NULL))
(defcfun ("cvGetMinMaxHistValue" get-min-max-hist-value) :void
  (hist :pointer)
  (min-value (:pointer :float))
  (max-value (:pointer :float))
  (min-idx (:pointer :int))
  (max-idx (:pointer :int)))

;; Calculates back project
;; void cvCalcArrBackProject(CvArr** image, CvArr* dst,
;;                           const CvHistogram* hist)
(defcfun ("cvCalcArrBackProject" calc-arr-back-project) :void
  (image :pointer)
  (dst :pointer)
  (hist :pointer))

;; Calculates array histogram
;; void cvCalcArrHist(CvArr** arr, CvHistogram* hist,
;;                    int accumulate CV_DEFAULT(0),
;;                    const CvArr* mask CV_DEFAULT(NULL))
(defcfun ("cvCalcArrHist" calc-arr-hist) :void
  (arr :pointer)
  (hist :pointer)
  (accumulate :int)
  (mask :pointer))

;; Creates new histogram
;; CvHistogram* cvCreateHist(int dims, int* sizes, int type,
;;                           float** ranges CV_DEFAULT(NULL),
;;                           int uniform CV_DEFAULT(1));
(defcfun ("cvCreateHist" create-hist) :pointer
  (dims :int)
  (sizes (:pointer :int))
  (type :int)
  (ranges :pointer)
  (uniform :int))


;;;;; tracking/


;; int cvCamShift(const CvArr* prob_image, CvRect window,
;;                CvTermCriteria criteria, CvConnectedComp* comp,
;;                CvBox2D* box CV_DEFAULT(NULL))
(defcfun ("cvCamShift" %camshift) :int
  (prob-image :pointer)
  (window rect)
  (criteria term-criteria)
  (comp :pointer)
  (box :pointer))

(defun camshift (prob-image window criteria comp box)
  "Implements CAMSHIFT algorithm to determine object position, size
and origentation from the object's histogram backprojection."
  (%camshift prob-image window criteria comp box))
