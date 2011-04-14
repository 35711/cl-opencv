;; (defpackage :cv-test
;;   (:use :cl :opencv-verrazano)
;;   (:import-from :cffi #:foreign-slot-value
;;                       #:null-pointer
;;                       #:defcallback)
;;   (:export #:test-tracking
;;            #:show-camera))

;; (in-package :cv-test)

(in-package :opencv-verrazano)

;; TODO
; Improve the tracking:
;; Test various objects
;; Occlude the object being tracked
;; Bring it in and out of frame
;; Make the ellipse tighter
; Improve style and make a proper package and/or system.
; Cleanup hist with release-hist after camshift-loop?

(defclass camshift-state ()
  ((drag-start :accessor drag-start
               :initform nil)
   (track-window :accessor track-window
                 :initform nil)
   (selection :accessor selection
              :initform nil)
   (comp :accessor comp
         :initform (foreign-alloc 'connected-comp))
   (track-box :accessor track-box
              :initform (foreign-alloc 'box-2d))
   (hist :accessor hist
         :initform (create-hist 1 180 +hist-array+))))

(defparameter *camshift-state* (make-instance 'camshift-state))

(defcallback on-mouse :void ((event :int) (x :int) (y :int)
                             (flags :int) (param :pointer))
  (declare (ignore flags param))
  (when (= event +event-lbuttondown+)
    (setf (drag-start *camshift-state*) (list x y)))
  (when (= event +event-lbuttonup+)
    (setf (drag-start *camshift-state*) nil
          (track-window *camshift-state*) (selection *camshift-state*)))
  (when (drag-start *camshift-state*)
    (let ((xmin (min x (first (drag-start *camshift-state*))))
          (ymin (min y (second (drag-start *camshift-state*))))
          (xmax (max x (first (drag-start *camshift-state*))))
          (ymax (max y (second (drag-start *camshift-state*)))))
      (setf (selection *camshift-state*)
            (list xmin ymin (- xmax xmin) (- ymax ymin))))))

(defun is-rect-nonzero (rect)
  (and (plusp (third rect))
       (plusp (fourth rect))))

(defun camshift-loop (&key window-name capture-src)
  (let* ((frame (query-frame capture-src))
         (hsv (create-image (get-size frame) 8 3))
         (backproject (create-image (get-size frame) 8 1))
         (hue (create-image (get-size frame) 8 1)))

    ;; Convert the current frame to HSV but keep the hue
    (cvt-color frame hsv +bgr-to-hsv+)
    (split hsv hue)
    (with-foreign-object (image-ptr :pointer)
      (setf (mem-ref image-ptr :pointer) hsv)
      (%release-image image-ptr))

    ;; Compute back projection and run the camshift
    (calc-arr-back-project hue backproject (hist *camshift-state*))
    (when (and (track-window *camshift-state*)
               (is-rect-nonzero (track-window *camshift-state*)))
      (let ((crit `(,(logior +termcrit-iter+ +termcrit-eps+) 10 1.0d0)))
        (with-accessors ((comp comp) (window track-window)
                         (track-box track-box)) *camshift-state*
          (camshift backproject window crit comp track-box)
          (setf window (fsbv:object (foreign-slot-value
                                     comp 'connected-comp 'rect) 'rect)))))
    (with-foreign-object (image-ptr :pointer)
      (setf (mem-ref image-ptr :pointer) backproject)
      (%release-image image-ptr))

    ;; Handle mouse input
    (if (and (drag-start *camshift-state*)
             (is-rect-nonzero (selection *camshift-state*)))

        ;; Highlight the selected area, recompute histogram
        (with-foreign-objects ((sub 'mat) (sel 'mat) (save 'mat))
          (setf sub (get-sub-rect frame sub (selection *camshift-state*))
                save (clone-mat sub))
          (convert-scale frame frame 0.5d0)
          (copy save sub)
          (destructuring-bind (x y w h) (selection *camshift-state*)
            (rectangle frame (list x y) (list (+ x w) (+ y h))
                       '(255.0d0 255.0d0 255.0d0 0.0d0)))
          (setf sel (get-sub-rect hue sel (selection *camshift-state*)))
          (calc-arr-hist sel (hist *camshift-state*) 0)
          (let ((max (second (get-min-max-hist-value (hist *camshift-state*)))))
            (unless (zerop max)
              (convert-scale (foreign-slot-value (hist *camshift-state*) 'histogram 'bins)
                             (foreign-slot-value (hist *camshift-state*) 'histogram 'bins)
                             (/ 255.0d0 max)))))

        ;; Draw the damn box and show it to the user already!
        (when (and (track-window *camshift-state*)
                   (is-rect-nonzero (track-window *camshift-state*)))
          (let* ((box (fsbv:object (track-box *camshift-state*) 'box-2d))
                 (track-box (list (mapcar #'round (fsbv:object (first box) 'point-2d-32f))
                                  (mapcar (lambda (x)
                                            (round (/ x 2)))
                                          (fsbv:object (second box) 'size-2d-32f))
                                  ;(- (/ (* 360 (third box)) (* 2 pi)) 90))))
                                  (third box))))
            (format t "What's track-box? ~A~%" track-box)
            ;(ellipse-box frame track-box '(255.0d0 0.0d0 0.0d0 0.0d0) 3 +aa+ 0))))
            (ellipse frame (first track-box) (second track-box)
                     (coerce (third track-box) 'double-float) 0.0d0 360.0d0
                     '(255.0d0 0.0d0 0.0d0 0.0d0) 1 +aa+ 0))))
    (with-foreign-object (image-ptr :pointer)
      (setf (mem-ref image-ptr :pointer) hue)
      (%release-image image-ptr))
    (show-image window-name frame)))

(defun reset-camshift ()
  (with-accessors ((comp comp) (track-box track-box)) *camshift-state*
    (foreign-free comp)
    (foreign-free track-box))
  (with-foreign-object (hist-ptr :pointer)
    (setf (mem-ref hist-ptr :pointer) (hist *camshift-state*))
    (%release-hist hist-ptr))
  (setf *camshift-state* (make-instance 'camshift-state)))

(defun command-char (char &key (quit-char #\q))
  (let ((cv-char (code-char (mod char 256))))
    (cond ((char= quit-char cv-char) t)
          ((char= #\n cv-char) (progn (reset-camshift) nil))
          ((char= #\Esc cv-char) (sb-ext:quit))
          (t nil))))

(defun test-tracking (&key (source 0) (window-name "Camshift Demo"))
  (with-window (window-name +window-autosize+)
    (with-video (video source)
      (format t "~%Keys:~%  To return to the REPL, press 'q'. ~
Click and drag with the mouse to select the object to track. ~
To track a new object, press 'n'. To exit, press 'Esc'.~%")
      ;; wait-key only works when a named-window exists.
      (set-mouse-callback window-name (callback on-mouse) (null-pointer))
      (unwind-protect
           (loop until (command-char (wait-key 33)) do
                (camshift-loop :window-name window-name :capture-src video))
        (reset-camshift)))))

(defun show-video (&key (source 0) (quit-char #\q)
                   (window-name "OpenCV Demo"))
  (with-window (window-name +window-autosize+)
    (with-video (video source)
      (format t "~%Keys:~%  To quit, press ~C.~%" quit-char)
      ;; wait-key appears to only work when a named-window is around.
      (loop until (char= quit-char (code-char (mod (wait-key 33) 256)))
         do (show-image window-name (query-frame video))))))

(defun test-tracking-gui ()
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:gtk-window
                                 :default-width 300
                                 :default-height 300
                                 :window-position :center
                                 :title "Camshift"))
          (menu-bar (make-instance 'gtk:menu-bar
                                   :pack-direction :ltr
                                   :child-pack-direction :ltr))
          (camera (make-instance 'gtk:menu-item
                                 :label "Camera"))
          (file (make-instance 'gtk:menu-item
                               :label "File"))
          (source (make-instance 'gtk:menu
                                 :attach-widget file
                                 :attach-widget camera))
          (track (make-instance 'gtk:menu
                                :label "Track"))
          (sbutton (make-instance 'gtk:button :label "Start"))
          (pbutton (make-instance 'gtk:button :label "Pause")))
      (gtk:container-add window menu-bar sbutton pbutton)
      (gtk:widget-show window :all t))))
