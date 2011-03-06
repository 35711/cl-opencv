; (in-package :cl-opencv-test)

(in-package :opencv-verrazano)

;;;; TODO
;; Get mouse callback going.
;; Do backprojections.
;; CAMSHIFT!

(defun show-camera (&key (device-id 0) (window-name "Camshift Demo"))
  (with-window (window-name +window-autosize+)
    (with-video (camera (create-camera-capture device-id))
      (format t "~%Keys:~%  To quit, press any key.~%")
      ;; wait-key appears to only work when a named-window is around. strange...
      (loop until (plusp (wait-key 33)) do
           (show-image window-name (query-frame camera))))))
  ;(set-mouse-callback "Camshift Demo" (callback on-mouse))
  ;; TODO: Add object for mouse state (drag-start, tracking-window)?

;; TODO!
;(defcallback on-mouse ())
