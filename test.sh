#!/bin/sh
sbcl --noinform --eval "(ql:quickload :cl-opencv-test)" \
     --eval "(cl-opencv-test:show-camera-threshold)" \
     --eval "(sb-ext:quit)"
exit 0

