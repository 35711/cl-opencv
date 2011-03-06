; (in-package :cl-opencv-test)

(in-package :opencv-verrazano)

;;;; TODO
;; CAMSHIFT!
;; - defun hue-histogram-as-image
;; - defun camshift

(defclass mouse-state ()
  ((drag-start :accessor drag-start
               :initform nil)
   (track-window :accessor track-window
                 :initform nil)
   (selection :accessor selection
              :initform nil)))

(defparameter *mouse-state* (make-instance 'mouse-state))

(defcallback on-mouse :void ((event :int) (x :int) (y :int)
                             (flags :int) (param :pointer))
  (declare (ignore flags param))
  (when (= event +event-lbuttondown+)
    (setf (drag-start *mouse-state*) (list x y)))
  (when (= event +event-lbuttonup+)
    (setf (drag-start *mouse-state*) nil
          (track-window *mouse-state*) (selection *mouse-state*)))
  (when (drag-start *mouse-state*)
    (let ((xmin (min x (first (drag-start *mouse-state*))))
          (ymin (min y (second (drag-start *mouse-state*))))
          (xmax (max x (first (drag-start *mouse-state*))))
          (ymax (max y (second (drag-start *mouse-state*)))))
      (setf (selection *mouse-state*)
            (list xmin ymin (- xmax xmin) (- ymax ymin))))))

(defun is-rect-nonzero (rect)
  (and (plusp (third rect))
       (plusp (fourth rect))))

(defun hue-histogram-as-image (hist)
  )

(defun camshift (&key window-name capture-src)
  )

(defun test-tracking (&key (device-id 0) (quit-char #\q)
                      (window-name "Camshift Demo"))
  (with-window (window-name +window-autosize+)
    (with-video (camera (create-camera-capture device-id))
      (format t "~%Keys:~%  To quit, press ~C.~%" quit-char)
      ;; wait-key only works when a named-window exists.
      (set-mouse-callback window-name (callback on-mouse) (null-pointer))
      (loop until (char= quit-char
                         (code-char (mod (wait-key 33) 256)))
         do ; (camshift :window-name window-name :capture-src camera)
           (show-image window-name (query-frame camera))))))

(defun show-camera (&key (device-id 0) (quit-char #\q)
                    (window-name "OpenCV Demo"))
  (with-window (window-name +window-autosize+)
    (with-video (camera (create-camera-capture device-id))
      (format t "~%Keys:~%  To quit, press ~C.~
Click and drag with the mouse to select the object to track.~%"
              quit-char)
      ;; wait-key appears to only work when a named-window is around.
      (loop until (char= quit-char (code-char (mod (wait-key 33) 256))) do
           (show-image window-name (query-frame camera))))))

