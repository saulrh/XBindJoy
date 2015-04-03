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
(use-modules (saulrh xbindjoy))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions
(define (display-n x)
  (display x) (newline))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; first things first: find our joystick and initialize bindings for it

(define jsd (jsname->device "DragonRise Inc.   Generic   USB  Joystick  "))
(if (string? jsd) 
    (display-n (string-append "found joystick" jsd))
    (begin
      (display-n "Couldn't find requested joystick")
      (quit)))

(init-xbindjoy (get-js-num-buttons jsd)
               (get-js-num-axes jsd))

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

(define do-axes-display #f)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; button callbacks
(bind-button `(press . ,bt-a) (lambda () (display "button a down 1\n")))
(bind-button `(release . ,bt-a) (lambda () (display "button a up 1\n")))

(bind-button `(press . ,bt-a) (lambda () (display "button a down 2\n")))
(bind-button `(release . ,bt-a) (lambda () (display "button a up 2\n")))

(bind-button `(press . ,bt-b) (lambda () (display "button b down\n")))
(bind-button `(release . ,bt-b) (lambda () (display "button b up\n")))


(bind-button `(press . ,bt-x) (lambda () (send-key 'press 'X 0)))
(bind-button `(release . ,bt-x) (lambda () (send-key 'release 'X 0)))

(bind-key-to-button bt-y 'Y)
(bind-button-to-button bt-lb 1)

(bind-button `(press . ,bt-start)
             (lambda () (set! do-axes-display (not do-axes-display))))

(bind-button `(press . ,bt-sel)
             (let ((seq (text->keyseq "select")))
               (lambda () 
                 (send-keyseq seq))))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; axis callbacks

(bind-axis (lambda (dt axes axes-last)
             (if do-axes-display
                 (begin
                   (display axes)
                   (newline)))))

(bind-axis (lambda (dt axes axes-last)
             (begin (if (ax-trans? axes axes-last ax-ly 0.7 #t)
                        (send-key 'press 'S 0))
                    (if (ax-trans? axes axes-last ax-ly 0.7 #f)
                        (send-key 'release 'S 0)))))
(bind-key-to-axis-region ax-ly -0.7 -1.2 'W)
(bind-key-to-axis-region ax-lx -0.7 -1.2 'A)
(bind-key-to-axis-region ax-lx  0.7  1.2 'D)

(define mousespeed 100)
(bind-button `(press . ,bt-rb)
             (lambda () (set! mousespeed 1000)))
(bind-button `(release . ,bt-rb)
             (lambda () (set! mousespeed 100)))

(bind-axis (lambda (dt axes axes-last)
             (let* ((lsx (assoc-ref axes ax-rx))
                    (lsy (assoc-ref axes ax-ry))
                    (vx (* lsx mousespeed))
                    (vy (* lsy mousespeed))
                    (dx (* vx dt))
                    (dy (* vy dt)))
               (begin
                 (format #t "mouse movement: ~a ~a\n" dx dy)
                 (send-mouserel dx dy)))))

(xbindjoy-start jsd)
(display "\n")
