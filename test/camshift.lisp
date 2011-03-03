; (in-package :cl-opencv-test)

(in-package :opencv-verrazano)

;;;; TODO
;; Fix release-capture/with-video.
;;; create-camera-capture returns CvCapture*, release-capture takes CvCapture**
;;; so we can't just pass in the return value of create-camera-capture.
;;; We have to hand over a pointer to the pointer. Lame.
;; Add with-output/window macro?
;; Figure out why wait-key won't work. Maybe just use trivial-raw-io?

(defun show-camera (&key (device-id 0) (window-name "Camshift Demo"))
  (with-video (camera (create-camera-capture device-id))
    (let ((window (named-window window-name +window-autosize+)))
      (format t "~%Keys:~%    q then Enter - Quit~%To initialize tracking, ~
click the left mouse button and drag to select your target.")
  ;(set-mouse-callback "Camshift Demo" (callback on-mouse))
  ;; TODO: Add object for mouse state (drag-start, tracking-window)?
      (loop until (char= #\q (read-char)) do
           (show-image window (query-frame camera)))
      (destroy-window window-name))))

;; TODO!
;(defcallback on-mouse ())

