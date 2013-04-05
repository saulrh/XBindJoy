#!../build/xbindjoy \
-s
!#

;;; load up some things to make everything cleaner
(use-modules (srfi srfi-1))
(include "helpers.scm")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; configuration stuff

(define sc-f1 0)
(define sc-f2 1)
(define sc-f3 2)
(define sc-f4 3)
(define sc-f5 4)
(define sc-f6 5)
(define sc-plus 6)
(define sc-minus 7)
(define sc-thtop 8)
(define sc-thmid 9)
(define sc-thbot 10)
(define sc-rec 11)
(define sc-sl1 12)
(define sc-sl2 13)
(define sc-sl3 14)


(define sc-x 0)
(define sc-y 1)
(define sc-r 2)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; variables

(define stratcom-key '())

(define last-axis-vals-alist `((,sc-x . 0) (,sc-y . 0) (,sc-r . 0)))
(define cur-axis-vals-alist `((,sc-x . 0) (,sc-y . 0) (,sc-r . 0)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions

(define (transition axis thresh to+)
  (let ((cur (assoc-ref cur-axis-vals-alist axis))
        (last (assoc-ref last-axis-vals-alist axis))
        (pred (if to+ > <)))
    (and (pred thresh last) (not (pred thresh cur)))))

(define (move-region axis a b key)
  (let ((lower (min a b))
        (upper (max a b)))
    (if (transition axis lower #t) (send-key 'press key 0))
    (if (transition axis lower #f) (send-key 'release key 0))
    (if (transition axis upper #t) (send-key 'release key 0))
    (if (transition axis upper #f) (send-key 'press key 0))))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple keybindings

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; more complicated stuff


(define mod-top #f)
(define mod-mid #f)
(define mod-bot #f)

(define (get-mod-top) mod-top)
(define (get-mod-mid) mod-mid)
(define (get-mod-bot) mod-bot)

(define-shift stratcom-key sc-thtop mod-top)
(define-shift stratcom-key sc-thmid mod-mid)
(define-shift stratcom-key sc-thbot mod-bot)

(define shift-down #f)
(define (toggle-shift)
  (if shift-down
      (send-key 'release 'Shift_L 0)
      (send-key 'press 'Shift_L 0))
  (set! shift-down (not shift-down)))

(define-key stratcom-key `(press . ,sc-f4)
  (lambda () (cond (mod-top '())
                   (mod-mid '())
                   (mod-bot '())
                   (else (toggle-shift)))))

(for-each (lambda (map-pair) (let* ((target (car map-pair))
                                    (result (cdr map-pair))
                                    (action (car target))
                                    (modifiers (cadr target))
                                    (key (caddr target)))
                               (define-key stratcom-key (cons action key)
                                        ;(lambda () (if (any values modifiers) (apply xbindjoy-send-key result)))
                                 (lambda () (begin (display (map (lambda (a) (apply (eval a (interaction-environment)) '())) modifiers))
                                                   (display result)
                                                   (newline)))
                                 )))
          `(((press () ,sc-f1) . (press J 0))
            ((release () ,sc-f1) . (release J 0))
            ((press (get-mod-top) ,sc-f1) . (press K 0))
            ((release (get-mod-top) ,sc-f1) . (release K 0))))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; axis handling

(define (stratcom-axis axis-vals-alist)
  (begin (set! cur-axis-vals-alist (scale-vals-alist axis-vals-alist))
         (move-region sc-x +0.2 +1.1 'D)
         (move-region sc-x -0.2 -1.1 'A)
         (move-region sc-y +0.2 +1.1 'S)
         (move-region sc-y -0.2 -1.1 'W)
         (if (transition sc-y -0.95 #f) (send-keyseq '((release W 0)
                                                                (press W 10)
                                                                (release W 20)
                                                                (press W 30))))
         ;(if mod-top (begin (display "mod-top") (newline)))
         (set! last-axis-vals-alist cur-axis-vals-alist)))

(define jsd (jsname->device "Microsoft Microsoft SideWinder Strategic Commander"))
(xbindjoy-start jsd stratcom-key stratcom-axis)


