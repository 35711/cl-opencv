;; (ql:quickload '(cffi fsbv))
;; I patched fsbv's libffi-unix.lisp to point to libffi like so:
;;#-darwin
;;(cc-flags "-I/usr/lib/libffi-3.0.9/include/")

;; This is obviously brittle. Where do debian et al put the header?


(defpackage :opencv-verrazano
  (:use :cl :cffi))

(in-package :opencv-verrazano)


;;;;; NOTES


;;; We actually can handle structs by value if needed with
;;; FSBV (Foreign Structures By Value), available through quicklisp.
;;; It adds a requirement on libffi and needed a patch on my system
;;; to help if find the libffi headers or it wouldn't build.

;; Takes at least one passed-by-value struct
; cvCreateImage create-image takes a CvSize
; cvGetSubRect get-sub-rect takes a CvRect
; cvCamShift %camshift takes CvTermCriteria and CvRect
; cvRectangle rectangle takes CvPoint and CvScalar
; cvEllipseBox ellipse-box takes CvBox2D and CvScalar
; cvSetMouseCallback set-mouse-callback takes CvMouseCallback
; // But CvMouseCallback is a void*. Does this count?

;; Returns a struct (not a pointer to struct)
; cvGetSize get-size returns a CvSize
; cvScalar scalar returns a CvScalar

;; Undefined alien (i.e. CV_INLINE) whiners
; cvScalar
; cvEllipseBox

;; TODO:
; Document the lisp wrappers that use with-foreign-*...

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


(fsbv:defcstruct size
  (width :int)
  (height :int))

(fsbv:defcstruct rect
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defctype mouse-callback :pointer)

(fsbv:defcstruct point
  (x :int)
  (y :int))

(fsbv:defcstruct scalar
  ;; b, g, r, alpha
  (val :double :count 4))

(fsbv:defcstruct point-2d-32f
  (x :float)
  (y :float))

(fsbv:defcstruct size-2d-32f
  (width :float)
  (height :float))

(fsbv:defcstruct box-2d
  (center point-2d-32f)
  (size size-2d-32f)
  (angle :float))

(fsbv:defcstruct term-criteria
  (type :int)
  (max-iter :int)
  (epsilon :double))

(fsbv:defcstruct mat-nd
  (type :int)
  (dims :int)
  (refcount :pointer)
  (data :pointer)
  (dim :pointer))

(fsbv:defcstruct histogram
  (type :int)
  (bins :pointer)
  (thresh :float)
  (thresh2 :pointer)
  (mat mat-nd))

(fsbv:defcstruct connected-comp
  (area :double)
  (value scalar)
  (rect rect)
  (contour :pointer))


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


;;;;; core/


;; Returns width and height of array in elements
;; CvSize cvGetSize(const CvArr* arr)
(fsbv:defcfun ("cvGetSize" get-size) size
  (arr :pointer))

;; Copies source array to destination array
;; void cvCopy(const CvArr* src, CvArr* dst,
;;             const CvArr* mask CV_DEFAULT(NULL))
(defcfun ("cvCopy" %copy) :void
  (src :pointer)
  (dst :pointer)
  (mask :pointer))

(defun copy (src dst &optional (mask (null-pointer)))
  (%copy src dst mask))

;; Retrieves number of an array dimensions and
;; optionally sizes of the dimensions
;; int cvGetDims(const CvArr* arr, int* sizes CV_DEFAULT(NULL))
(defcfun ("cvGetDims" %get-dims) :int
  (arr :pointer)
  (sizes (:pointer :int)))

(defun get-dims (arr &optional (sizes (null-pointer)))
  (%get-dims arr sizes))

;; Splits a multi-channel array into the set of single-channel
;; arrays or extracts particular [color] plane
;; void cvSplit(const CvArr* src, CvArr* dst0, CvArr* dst1,
;;              CvArr* dst2, CvArr* dst3)
(defcfun ("cvSplit" %split) :void
  (src :pointer)
  (dst-0 :pointer)
  (dst-1 :pointer)
  (dst-2 :pointer)
  (dst-3 :pointer))

(defun split (src dst0 &optional (dst1 (null-pointer))
              (dst2 (null-pointer)) (dst3 (null-pointer)))
  (%split src dst0 dst1 dst2 dst3))

;; Creates IPL image (header and data)
;; IplImage* cvCreateImage(CvSize size, int depth, int channels)
(fsbv:defcfun ("cvCreateImage" create-image) :pointer
  (size size)
  (depth :int)
  (channels :int))

;; Makes a new matrix from <rect> subrectangle of input array.
;; No data is copied
;; CvMat* cvGetSubRect(const CvArr* arr, CvMat* submat, CvRect rect)
(fsbv:defcfun ("cvGetSubRect" get-sub-rect) :pointer
  (arr :pointer)
  (submat :pointer)
  (rect rect))

;; Finds global minimum, maximum and their positions
;; void cvMinMaxLoc(const CvArr* arr, double* min_val, double* max_val,
;;                  CvPoint* min_loc CV_DEFAULT(NULL),
;;                  CvPoint* max_loc CV_DEFAULT(NULL),
;;                  const CvArr* mask CV_DEFAULT(NULL))
(defcfun ("cvMinMaxLoc" %min-max-loc) :void
  (arr :pointer)
  (min-val (:pointer :double))
  (max-val (:pointer :double))
  (min-loc :pointer)
  (max-loc :pointer)
  (mask :pointer))

(defun min-max-loc (arr &optional (min-val 0.0) (max-val 0.0)
                    (min-loc (null-pointer)) (max-loc (null-pointer))
                    (mask (null-pointer)))
  (with-foreign-objects ((minval :double)
                         (maxval :double))
    (setf (mem-ref minval :double) min-val)
    (setf (mem-ref maxval :double) max-val)
    (%min-max-loc arr minval maxval min-loc max-loc mask)
    (list minval maxval min-loc max-loc)))

;; Performs linear transformation on every source array element:
;;    dst(x,y,c) = scale*src(x,y,c)+shift.
;; Arbitrary combination of input and output array depths are
;; allowed (number of channels must be the same), thus the
;; function can be used for type conversion
;; void cvConvertScale(const CvArr* src, CvArr* dst,
;;                     double scale CV_DEFAULT(1),
;;                     double shift CV_DEFAULT(0))
(defcfun ("cvConvertScale" %convert-scale) :void
  (src :pointer)
  (dst :pointer)
  (scale :double)
  (shift :double))

(defun convert-scale (src dst scale &optional (shift 0.0))
  (%convert-scale src dst scale shift))

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
(fsbv:defcfun ("cvRectangle" %rectangle) :void
  (img :pointer)
  (pt-1 point)
  (pt-2 point)
  (color scalar)
  (thickness :int)
  (line-type :int)
  (shift :int))

(defun rectangle (img pt1 pt2 color thickness
                  &optional (line-type 0) (shift 0))
  (%rectangle img pt1 pt2 color thickness line-type shift))

;; TODO: This is throwing "undefined alien" style-warnings
;; because it's inlined. Fix it. (later) Actually it's worse
;; than that. This errors completely when defined with
;; fsbv:defcfun but that's exactly what we need since it
;; takes structures by value... uh-oh...
;; void cvEllipseBox(CvArr* img, CvBox2D box, CvScalar color,
;;                  int thickness CV_DEFAULT(1),
;;                  int line_type CV_DEFAULT(8),
;;                  int shift CV_DEFAULT(0))
(fsbv:defcfun ("cvEllipseBox" ellipse-box) :void
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

;; CvScalar cvScalar(double val0, double val1 CV_DEFAULT(0),
;;                   double val2 CV_DEFAULT(0),
;;                   double val3 CV_DEFAULT(0))
(defcfun ("cvScalar" scalar) scalar
  (val1 :double)
  (val2 :double)
  (val3 :double)
  (val4 :double))

(defun rgb (r g b)
  (scalar b g r 0))


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
(defcfun ("cvGetMinMaxHistValue" %get-min-max-hist-value) :void
  (hist :pointer)
  (min-value (:pointer :float))
  (max-value (:pointer :float))
  (min-idx (:pointer :int))
  (max-idx (:pointer :int)))

(defun get-min-max-hist-value (hist &optional (min-val 0.0) (max-val 1.0)
                               (min-idx (null-pointer)) (max-idx (null-pointer)))
  (with-foreign-objects ((minval :float)
                         (maxval :float))
    (setf (mem-ref minval :float) min-val)
    (setf (mem-ref maxval :float) max-val)
    (%get-min-max-hist-value hist minval maxval min-idx max-idx)
    (list minval maxval min-idx max-idx)))

;; Calculates back project
;; void cvCalcArrBackProject(CvArr** image, CvArr* dst,
;;                           const CvHistogram* hist)
(defcfun ("cvCalcArrBackProject" %calc-arr-back-project) :void
  (image :pointer)
  (dst :pointer)
  (hist :pointer))

(defun calc-arr-back-project (image dst hist)
  (with-foreign-object (ptr-to-image :pointer)
    (setf (mem-ref ptr-to-image :pointer) image)
    (%calc-arr-back-project ptr-to-image dst hist)))

;; Calculates array histogram
;; void cvCalcArrHist(CvArr** arr, CvHistogram* hist,
;;                    int accumulate CV_DEFAULT(0),
;;                    const CvArr* mask CV_DEFAULT(NULL))
(defcfun ("cvCalcArrHist" %calc-arr-hist) :void
  (arr :pointer)
  (hist :pointer)
  (accumulate :int)
  (mask :pointer))

(defun calc-arr-hist (arr hist acc &optional (mask (null-pointer)))
  (with-foreign-object (ptr-to-arr :pointer)
    (setf (mem-ref ptr-to-arr :pointer) arr)
    (%calc-arr-hist ptr-to-arr hist acc mask)))

;; Creates new histogram
;; CvHistogram* cvCreateHist(int dims, int* sizes, int type,
;;                           float** ranges CV_DEFAULT(NULL),
;;                           int uniform CV_DEFAULT(1));
(defcfun ("cvCreateHist" %create-hist) :pointer
  (dims :int)
  (sizes (:pointer :int))
  (type :int)
  (ranges :pointer)
  (uniform :int))

(defun create-hist (dims sizes type ranges &optional (uniform 1))
  (with-foreign-objects ((sizes-ptr :int)
                         (ranges-ptr :float (length ranges)))
    (setf (mem-ref sizes-ptr :int) sizes)
    (dotimes (i (length ranges))
      (setf (mem-aref ranges-ptr :float i) (aref ranges i)))
    (%create-hist dims sizes-ptr type ranges-ptr uniform)))


;;;;; tracking/


;; int cvCamShift(const CvArr* prob_image, CvRect window,
;;                CvTermCriteria criteria, CvConnectedComp* comp,
;;                CvBox2D* box CV_DEFAULT(NULL))
(fsbv:defcfun ("cvCamShift" %camshift) :int
  (prob-image :pointer)
  (window rect)
  (criteria term-criteria)
  (comp :pointer)
  (box :pointer))

(defun camshift (prob-image window criteria)
  "Implements CAMSHIFT algorithm to determine object position, size
and origentation from the object's histogram backprojection."
  (with-foreign-objects ((comp 'connected-comp)
                         (track-box 'box-2d))
    (%camshift prob-image window criteria comp track-box)
    (list comp track-box)))
