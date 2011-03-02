; (in-package :cl-opencv-test)

(in-package :opencv-verrazano)

(defparameter *video* nil
  "The video input to be processed.")
(defparameter *output* nil
  "The output destination of processed video.")

;; TODO: Source should be a seq with keyword naming source as
;; first element (i.e. :webcam, :stream, :file) and data as second
;; (i.e. 0, "http://...", "/home/$user/...")
(defun init (&key (source :webcam))
  (load-opencv-libs)
  (if (eql source :webcam)
      (setf *video* (create-camera-capture 0))
      (format t "Only webcam input supported at this time.~%"))
  (setf *output* (named-window "Camshift Demo" 1))
  ;(set-mouse-callback "Camshift Demo" (callback on-mouse))
  ;; TODO: Add object for mouse state (drag-start, tracking-window)?
  (format t "~%Keys:~%    ESC - Quit~%To initialize tracking, ~
click the left mouse button and drag to select your target."))

(defun main ()
  (init)
  (loop for key = (mod (wait-key 7) 256) until (= key 27) do
       (format t "It works!~%")))

;; TODO!
;(defcallback on-mouse ())

