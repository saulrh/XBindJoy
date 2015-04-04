#!/usr/bin/guile \
-s
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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; first things first: find our joystick and initialize bindings for it


;; ;;; here's how you'd find a joystick by name and grab it. There's a function "jsname->device" which
;; ;;; takes the name of a joystick and searches through the numbered joystick devices in /dev/input/
;; ;;; to find one with the right name, then returns the path to that joystick.

;; (define jsname "DragonRise Inc.   Generic   USB  Joystick  ")
;; (define jsd (jsname->device jsname))
;; (if (string? jsd) 
;;     (display-n (string-append "found joystick" jsd))
;;     (begin
;;       (format #t "Couldn't find joystick ~s\n" jsname)
;;       (quit)))

;;; but we're not going to do that, because this is an example that we want people to be able to
;;; run right after they grab the code. so we just use the first joystick that's plugged in.
(define jsd "/dev/input/js0")

(define naxes (get-js-num-axes jsd))
(define nbuttons (get-js-num-buttons jsd))
(init-xbindjoy nbuttons naxes)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assign symbols to buttons. 
;;; 
;;; if you get a new joystick and don't want to change your code too mcuh, you can change these
;;; around to match the joystick easily

;;; convenient names for axes.
(define ax-lx 0)                        ;left stick x axis
(define ax-ly 1)                        ;left stick y axis
(define ax-rx 3)                        ;right stick x axis
(define ax-ry 4)                        ;right stick y axis
(define ax-dx 5)                        ;d-pad x axis
(define ax-dy 6)                        ;d-pad y axis

(if (< naxes 5)
    (format #t
"ERROR: AXISCOUNT: Your joystick has fewer axes than this example wants (~a vs ~a). You will
probably have to go into the example and fiddle with the axis numbering information.\n" 5 naxes))

;;; convenient names for buttons
(define bt-a 2)                         ;face a button
(define bt-b 1)                         ;face b button
(define bt-x 3)                         ;... x
(define bt-y 0)                         ;... y
(define bt-start 9)                     ;start button
(define bt-sel 8)                       ;select button

(define bt-lb 6)                        ;left bumper
(define bt-rb 7)                        ;right bumper
(define bt-lt 4)                        ;left trigger
(define bt-rt 5)                        ;right trigger


;;; first we demonstrate the fundamental feature: binding a function to a button
;;; press. bind-button->proc takes in an action - in this case the pair ('press 2), which means
;;; "when the button with index 2 is pressed down" - and a function, and it runs the function
;;; whenever the specified action occurs.
(define (callback-button-a-press)
  (display "button a down\n"))
(bind-button->proc (cons 'press bt-a) callback-button-a-press)

;;; same thing, but when the button is released. if you're not familiar with lisp, this might be a
;;; bit opaque, so I'll explain. we're using a feature called "anonymous functions", which lets us
;;; define functions that we're going to use for callbacks without cluttering things up. the
;;; function 'lambda' returns a function that does the stuff you told it to. so we create a
;;; fucntion that just displays our thing, then hand it off to bind-button->proc without giving it
;;; a name or anything.
(bind-button->proc (cons 'release bt-a)
                   (lambda ()
                     (display "button a up\n")))

;;; nice feature here that not many other macro tools can handle: we can do multiple things on ever
;;; action. here we'll trigger two separate functions on the same action.
(bind-button->proc `(press . ,bt-b) (lambda () (display "button b down 1\n")))
(bind-button->proc `(press . ,bt-b) (lambda () (display "button b down 2\n")))

(bind-button->proc `(release . ,bt-b) (lambda () (display "button b up 1\n")))
(bind-button->proc `(release . ,bt-b) (lambda () (display "button b up 2\n")))

;;; here's the next fundamental feature: xbindjoy can send fake X events, like key or mouse button
;;; presses.
(bind-button->proc `(press . ,bt-x) (lambda () (send-key 'press 'X 0)))
(bind-button->proc `(release . ,bt-x) (lambda () (send-key 'release 'X 0)))

(bind-button->key bt-y 'Y)

(bind-button->mbutton bt-lb 1)

(bind-button->proc `(press . ,bt-sel)
                   (let ((seq (text->keyseq "select")))
                     (lambda () 
                       (send-keyseq seq))))


(bind-button->proc `(press . ,bt-rb)
                   (build-send-key-toggler 'Shift_L #f))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; axis callbacks

;;; we'll use this 
(define do-axes-display #f)
(bind-button->proc `(press . ,bt-start)
                   (lambda () (set! do-axes-display (not do-axes-display))))
(bind-axis->proc (lambda (dt axes axes-last)
                   (if do-axes-display
                       (begin
                         (display axes)
                         (newline)))))

(bind-axis->proc (lambda (dt axes axes-last)
                   (begin (if (ax-trans? axes axes-last ax-ly 0.7 #t)
                              (send-key 'press 'S 0))
                          (if (ax-trans? axes axes-last ax-ly 0.7 #f)
                              (send-key 'release 'S 0)))))
(bind-axis-region->key ax-ly -0.7 -1.2 'W)
(bind-axis-region->key ax-lx -0.7 -1.2 'A)
(bind-axis-region->key ax-lx  0.7  1.2 'D)

(define mousespeed 100)
(bind-button->proc `(press . ,bt-rb)
                   (lambda () (set! mousespeed 1000)))
(bind-button->proc `(release . ,bt-rb)
                   (lambda () (set! mousespeed 100)))

(bind-axis->proc (lambda (dt axes axes-last)
                   (let* ((lsx (assoc-ref axes ax-rx))
                          (lsy (assoc-ref axes ax-ry))
                          (vx (* lsx mousespeed))
                          (vy (* lsy mousespeed))
                          (dx (* vx dt))
                          (dy (* vy dt)))
                     (send-mouserel dx dy))))




(if (not (xbindjoy-start jsd))
    (format #t "Failed to start xbindjoy event loop, quitting now\n"))
(display "\n")
