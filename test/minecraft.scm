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
;;; define some shorthand for a strategic commander

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

(define mod-top #f)
(define mod-mid #f)
(define mod-bot #f)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions

(define (sign x)
  (if (positive? x) 1 -1))

(define (display-n x)
  (display x) (newline))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple keybindings

;;; bind our mod variables to the appropriate keys
(define-shift stratcom-key sc-thtop mod-top)
(define-shift stratcom-key sc-thmid mod-mid)
(define-shift stratcom-key sc-thbot mod-bot)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; more complicated stuff

(define toggle-shift (build-send-key-toggler 'Shift_L #f))

(define-key-when stratcom-key (cons 'press sc-f1) (list)
  (display-n "sc-f1 down"))
(define-key-when stratcom-key (cons 'release sc-f1) (list)
  (display-n "sc-f1 up"))
(define-key-when stratcom-key (cons 'press sc-f4) (list (not mod-top) (not mod-mid) (not mod-bot))
  (toggle-shift))
(define-key-when stratcom-key (cons 'press sc-f2) (list mod-top mod-mid)
  (display-n "mod-top, mod-mid"))
(define-key-when stratcom-key (cons 'press sc-f3) (list mod-top (not mod-mid))
  (display-n "mod-top, !mod-mid"))
(define-key-when stratcom-key (cons 'press sc-f5) (list (not mod-mid))
  (display-n "!mod-mid"))
(define-key-when stratcom-key (cons 'press sc-f6) (list)
  (begin (send-key 'press 'A 0)
         (send-key 'release 'A 5)
         (send-key 'press 'B 10)
         (send-key 'release 'B 5)
         (send-key 'press 'C 10)
         (send-key 'release 'C 5)
         (send-key 'press 'D 10)
         (send-key 'release 'D 5)
         (send-key 'press 'E 10)
         (send-key 'release 'E 5)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; axis handling

(set! last-axis-vals-alist `((,sc-x . 0) (,sc-y . 0) (,sc-r . 0)))
(set! cur-axis-vals-alist `((,sc-x . 0) (,sc-y . 0) (,sc-r . 0)))

(display axis-dt-nominal) (newline)

(define stratcom-axis (axesfun-with-history
                       (axis-region sc-x +0.2 +1.1 'D)
                       (axis-region sc-x -0.2 -1.1 'A)
                       (axis-region sc-y +0.2 +1.1 'S)
                       (axis-region sc-y -0.2 -1.1 'W)
                       (let ((x-motion
                              (* 300 axis-dt (expt (assoc-ref cur-axis-vals-alist sc-r) 3))))
                         (send-mouserel x-motion 0)
                         (format #t "dt:~a, x movement=~a\n" axis-dt x-motion)
                         )
                       (if (axis-transition sc-y -0.95 #f) (send-keyseq '((release W 0)
                                                                          (press W 10)
                                                                          (release W 10)
                                                                          (press W 10))))
                       
                       ;; (if mod-top (begin (display "mod-top")))
                       ;; (if mod-mid (begin (display "mod-mid")))
                       ;; (if mod-bot (begin (display "mod-bot")))
                       ))

;; (pretty-print stratcom-key) (newline)

(define jsd (jsname->device "Microsoft Microsoft SideWinder Strategic Commander"))
(xbindjoy-start jsd stratcom-key stratcom-axis)


