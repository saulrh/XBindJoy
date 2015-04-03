#!/usr/bin/guile \
--no-auto-compile -s
!#

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; better list-processing stuff
(use-modules (srfi srfi-1))

;;; better printing
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 format))

;;; and finally the xbindjoy library
(add-to-load-path (dirname (current-filename)))
(use-modules (saulrh xbindjoy))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assign symbols to buttons. 
;;; modify these to use different joysticks without changing your code

;;; axes
(define ax-lx 0)                          ;left stick x axis
(define ax-ly 1)                          ;left stick y axis
(define ax-rx 3)                          ;right stick x axis
(define ax-ry 4)                          ;right stick y axis

;;; face buttons
(define bt-a 2)
(define bt-b 1)
(define bt-x 3)
(define bt-y 0)
(define bt-start 9)
(define bt-sel 8)

;;; bumpers and triggers
(define bt-lb 6)
(define bt-rb 7)
(define bt-tb 4)
(define bt-tb 5)

;; ;;; d-pad ends up as buttons
;; (define bt-dup 8)
;; (define bt-ddown 9)
;; (define bt-dleft 10)
;; (define bt-dright 11)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; variables

(define js-key '())

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions

(define (display-n x)
  (display x) (newline))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; button handling

(define-key js-key `(press . ,bt-a)
  (lambda () 
    (display-n "button a down")))
(define-key js-key `(release . ,bt-a)
  (lambda () 
    (display-n "button a up")))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; axis handling
(define js-axis (lambda (dt axes-raw)
                  (let ((axes (normalize-jsaxes axes-raw)))
                    (display-n axes))))

(define jsd (jsname->device "DragonRise Inc.   Generic   USB  Joystick  "))
(xbindjoy-start jsd js-key js-axis)