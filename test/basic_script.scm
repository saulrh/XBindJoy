#!/usr/bin/guile \
-s
!#
; a basic test of the xbindjoy guile

(display "--------------------------------------")
(newline)
(define xbindjoy-lib (dynamic-link "../build/.libs/libguile-xbindjoy.so"))
(dynamic-call "init_xbindjoy" xbindjoy-lib)



(display (device->jsname "/dev/input/js0")) (newline)



