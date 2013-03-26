#!../build/xbindjoy \
-s
!#

;;; do some module loads
(use-modules (ice-9 pretty-print))

;;; this is the alist we'll use to store our key map
(define stratcom '())

;;; If I can find a sane way to handle initscripts for an extended
;;; guile interpreter, these should be moved to a global initscript,
;;; kind of like how shells have /etc/profile.
(define-syntax define-key
  (syntax-rules ()
    ((define-key keymap key func)
     (set! keymap (assoc-set! keymap key func)))))

(define (xbindjoy-send-keyseq keylist)
  (begin (xbindjoy-send-key (car keylist))
         (if (not (null? (cdr keylist)))
             (xbindjoy-send-keyseq (cdr keylist)))))

(define-syntax bind-key 
  (syntax-rules ()
    ((bind-key keymap buttoncode keycode)
     (begin (define-key keymap '(press . buttoncode) (lambda () (xbindjoy-send-key '(press . keycode))))
            (define-key keymap '(release . buttoncode) (lambda () (xbindjoy-send-key '(release . keycode))))))))

(define* (jsname->device name #:optional n)
  (if (not n)
      (jsname->device name 0)
      (let ((candidate (string-append "/dev/input/js" (number->string n))))
        (if (access? candidate R_OK)
            (if (equal? name (device->jsname candidate))
                candidate
                (jsname->device name (+ 1 n)))
            #f))))

;;; map a button directly to a key - send a keydown event whenever we
;;; get a button 2 down and a keyup whenever we get a button 2 up.
(define-key stratcom '(press . 1) (lambda () (xbindjoy-send-key '(press . L))))
(define-key stratcom '(release . 1) (lambda () (xbindjoy-send-key '(release . L))))

;;; as above, but we use the bind-key special form to automate the syntax
(bind-key stratcom 2 "K")

;;; something more complicated - remember the state of button 3 and
;;; use to to make button 4 do different things if it's pressed with
;;; or without button 3 down.
(define button-2-down #f)
(define-key stratcom '(press . 3) (lambda () (set! button-2-down #t)))
(define-key stratcom '(release . 3) (lambda () (set! button-2-down #f)))
(define-key stratcom '(press . 4)
  (lambda () (if button-2-down
                 (lambda () (xbindjoy-send-keyseq '((press . Shift_L)
                                                     (press . K)
                                                     (release . K)
                                                     (release . Shift_L))))
                 (lambda () (xbindjoy-send-keyseq '((press . K)
                                                    (release . K)))))))

;;; and finally we feed xbindjoy our keymap and the joystick device to
;;; xbindjoy so it can start processing. Make sure that this is the
;;; last thing you call - it will never return!
(define temp (cons (jsname->device "Microsoft Microsoft SideWinder Strategic Commander") stratcom))
(pretty-print temp) (newline)
;; (xbindjoy-start temp)
