#!../build/xbindjoy \
-s
!#

;;; load up some things to make everything cleaner
(include "helpers.scm")

;;; do some module loads
(use-modules (ice-9 pretty-print))

;;; this is the alist we'll use to store our key map
(define stratcom-key '())

;;; map a button directly to a key - send a keydown event whenever we
;;; get a button 2 down and a keyup whenever we get a button 2 up.
(define-key stratcom-key '(press . 1) (lambda () (send-key 'press 'L 0)))
(define-key stratcom-key '(release . 1) (lambda () (send-key 'release 'L 0)))

;;; as above, but we use the bind-key special form to automate the syntax
(bind-key stratcom-key '2 'K 0)

;;; we can also map to mouse buttons
(define-key stratcom-key '(press . 0) (lambda () (send-button 'press '1 0)))
(define-key stratcom-key '(release . 0) (lambda () (send-button 'release '1 0)))

;;; something more complicated - remember the state of button 3 and
;;; use to to make button 4 do different things if it's pressed with
;;; or without button 3 down.
(define button-3-down #f)
(define-key stratcom-key '(press . 3) (lambda () (set! button-3-down #t)))
(define-key stratcom-key '(release . 3) (lambda () (set! button-3-down #f)))
(define-key stratcom-key '(press . 4)
  (lambda () (if button-3-down
                 (send-keyseq '((press Shift_L 0)
                                         (press K 0)
                                         (release K 0)
                                         (release Shift_L 0)))
                 (send-keyseq '((press K 0)
                                         (release K 0))))))

(define (sign x)
  (if (positive? x) 1 -1))

(define (stratcom-axis axis-vals-alist)
  (let ((x (assoc-ref axis-vals-alist 0))
        (y (assoc-ref axis-vals-alist 1))
        (z (assoc-ref axis-vals-alist 2)))
    (begin (send-mouserel
            (* 30 (sign x) (expt (/ x 32768) 2))
            (* 30 (sign y) (expt (/ y 32768) 2))))))

;;; and finally we feed xbindjoy our keymap and the joystick device to
;;; xbindjoy so it can start processing. Make sure that this is the
;;; last thing you call - it will never return!
(define jsd (jsname->device "Microsoft Microsoft SideWinder Strategic Commander"))
;; (display jsd) (newline)
;; (pretty-print stratcom-key)
(xbindjoy-start jsd stratcom-key stratcom-axis)
