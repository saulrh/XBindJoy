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
;;; press. bind-button->proc takes in an action - in this case the pair ('press . 2), which means
;;; "button with index 2 is pressed" - and a function, and it runs the function whenever the
;;; specified action occurs.
(define (callback-button-a-press)
  (display "button a down\n"))
(bind-button->proc (cons 'press bt-a) callback-button-a-press)

;;; same thing, but with cleaner code. if you're not familiar with lisp, this might be a bit
;;; opaque, so I'll explain. we're using a feature called "anonymous functions", which lets us
;;; define functions that we're going to use for callbacks without cluttering things up. the
;;; function 'lambda' returns a function that does the stuff you told it to. so we create a
;;; fucntion that just displays our thing, then hand it off to bind-button->proc without giving it
;;; a name or anything.
(bind-button->proc (cons 'release bt-a)
                   (lambda ()
                     (display "button a up\n")))

;;; nice feature here that not many other macro tools can handle: we can do multiple things on
;;; every action. here we'll trigger two separate functions on the same action. procedures are
;;; called in reverse order because they're added to the front of a linked list which is processed
;;; in order on every event. whatever procedure you added last is called first when its action
;;; happens.
(bind-button->proc (cons 'press bt-b) (lambda () (display "button b down 1\n")))
(bind-button->proc (cons 'press bt-b) (lambda () (display "button b down 2\n")))

(bind-button->proc (cons 'release bt-b) (lambda () (display "button b up 1\n")))
(bind-button->proc (cons 'release bt-b) (lambda () (display "button b up 2\n")))

;;; here's the next fundamental feature: xbindjoy can send fake X events, like key or mouse button
;;; presses. send-key takes as arguments an action, a key to do it on, and a delay in ms. it uses
;;; the X11 testing api to send a synthetic X event saying to do the action with the specified
;;; key. Here we bind joystick button x to keyboard key x by pressing keyboard X whenever the
;;; button X is pressed and releasing keyboard x it whenever the button is released.
(bind-button->proc (cons 'press bt-x) (lambda () (send-key 'press 'X 0)))
(bind-button->proc (cons 'release bt-x) (lambda () (send-key 'release 'X 0)))

;;; and since that's such a common feature, I have a library function to do both of the above
;;; bindings for you in a simple way. this binds the keyboard key Y to the joystick button
;;; Y.
;;; 
;;; USERS WHO DON'T WANT MUCH FUSS: this function binds a joystick button directly to a keyboard
;;; button. Copy and paste it as necessary.
(bind-button->key bt-y 'Y)

;;; same thing, but with a mouse button. there's a function "send-mbutton" which sends a mouse
;;; click. same function signature as send-key but with a mouse button instead of a keyboard
;;; key. bind-button->mbutton is the same as bind-button->key in that it adds one binding for press
;;; and another for release.
(bind-button->mbutton bt-lb 1)

;;; here we see the use of send-keyseq and text->keyseq. send-keyseq takes a list of pairs that are
;;; the arguments to send-key and feeds them to send-key all at once: (('press 'X 0) ('release 'X
;;; 0)) will execute (send-key 'press 'X 0) (send-key 'release 'X 0). text-keyseq takes in a string
;;; and generates a list of key presses and releases that'll type that string.
(bind-button->proc (cons 'press bt-sel)
                   (let ((seq (text->keyseq "select")))
                     ;; for lisp newbies: "let" creates a new scope with the specified bindings, in
                     ;; this case that "seq" will be the result of the call (text->keyseq
                     ;; "select"). because lisp does closures right, this means that the lambda
                     ;; inside it will remember the scope of the "let", with the predefined seq,
                     ;; and use it when it's called when the select button press event goes off. so
                     ;; this construction lets us do the (text->keyseq) call /once/ rather than
                     ;; every time the select button is pressed.
                     (lambda () 
                       (send-keyseq seq))))

;;; here's another convenient library function. build-send-key-toggler takes in a key and returns a
;;; function which, when called, will alternate between sending a press event for tha tkey and a
;;; release event for that key:
;;; 
;;; (define tog (build-send-key-toggler 'A #f))
;;; (tog) ; sends press A
;;; (tog) ; send release A
;;; (tog) ; send press A
;;; (tog) ; send release A
;;; 
;;; this happens when it gets called through a binding, too. so this binding here is basically caps
;;; lock. great for minecraft: never fall into lava again!
(bind-button->proc (cons 'press bt-rb)
                   (build-send-key-toggler 'Shift_L #f))

;;; we create a function which grabs do-axes-display and toggles it. whenever this function is
;;;called it toggles the value of do-axes-display, including when it's called by an action binding.
(define do-axes-display #f)
(bind-button->proc (cons 'press bt-start)
                   (lambda () (set! do-axes-display (not do-axes-display))))

;;; now we do axes. another fundamental feature: bind-axis->proc calls the given procedure every
;;; fiftieth of a second with argumetns of the time since the last call, the current values of the
;;; joystick's axes, and the values of hte joystick's axes as of the last tick. axis values are
;;; doubles in the range (-1, 1), with 0 being neutral. here we check to see if do-axes-display is
;;; true, and if it is we print out the current value of our axes.
(bind-axis->proc (lambda (dt axes axes-last)
                   (if do-axes-display
                       (begin
                         (display axes)
                         (newline)))))

;;; just like with button bindings, you can have as many axis bindings as you want. there's a
;;; convenience function ax-trans? which, when given the axes, the last value, the axis we want,
;;; and a threshold, will return true if and only if the joystick crossed the threshold on this
;;; tick. so here, when the axis ax-ly moves across 0.7 toward the positive direction, we press S,
;;; and when it moves across 0.7 toward the negative direction, we release S.
(bind-axis->proc (lambda (dt axes axes-last)
                   (begin (if (ax-trans? axes axes-last ax-ly 0.7 #t)
                              (send-key 'press 'S 0))
                          (if (ax-trans? axes axes-last ax-ly 0.7 #f)
                              (send-key 'release 'S 0)))))

;;; and since that's such a common use case, we again have a convenience function that does it for
;;; us. bind-axis-region->key, given an axis, a key, and a region, sets up bindings such that the
;;; key is down when that axis is inside that region. note that here we put one bound at infinity
;;; to make sure we get the entire extreme end of the joystick axis.
;;; 
;;; USERS WHO DON'T WANT MUCH FUSS: this function binds the extremes of a joystick axis to keyboard
;;; keys. Copy and paste it as necessary.
(bind-axis-region->key ax-ly -0.7 -inf.0 'W)
(bind-axis-region->key ax-lx -0.7 -inf.0 'A)
(bind-axis-region->key ax-lx  0.7 +inf.0 'D)

;;; do the toggle thing again, this time to figure out how fast we want our mouse to go (units are
;;; pixels per second).
(define mousespeed 100)
(bind-button->proc ('press bt-rb) (lambda () (set! mousespeed 1000)))
(bind-button->proc ('release bt-rb) (lambda () (set! mousespeed 100)))

;;; here we have a function that moves the mouse around. the axes values are stored as an
;;; association list; think of it as an inefficient but /extremely/ simple dictionary. we pull out
;;; the values for the left stick's x and y axes using assoc-ref (dimensionless). we use those to
;;; calculate our desired mouse velocities (pixels per second). we then multiply by dt to get how
;;; many pixels we should move. we then use another fundamental function of libxbindjoy:
;;; send-mouserel generates synthetic mouse movement with the given x and y pixel values.
(bind-axis->proc (lambda (dt axes axes-last)
                   (let* ((lsx (assoc-ref axes ax-rx))
                          (lsy (assoc-ref axes ax-ry))
                          (vx (* lsx mousespeed))
                          (vy (* lsy mousespeed))
                          (dx (* vx dt))
                          (dy (* vy dt)))
                     (send-mouserel dx dy))))


;;; finally, we start the actual event loop. everything up to here has just been setup, this
;;; actaully starts listening to the joystick and calling back to the procedures we've been putting
;;; down everywhere. takes as an argument the path to a joystick device, something like
;;; /dev/input/js0.
(if (not (xbindjoy-start jsd))
    (format #t "Failed to start xbindjoy event loop, quitting now\n"))
(display "\n")
