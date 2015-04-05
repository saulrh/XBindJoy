;;; xbindjoy.scm: module definition file and some higher-level functions
;;; 
;;;  Copyright 2015 Saul Reynolds-Haertle.
;;; 
;;;  This file is part of XBindJoy. 
;;;  
;;;  XBindJoy is free software: you can redistribute it and/or modify 
;;;  it under the terms of the GNU General Public License as published by 
;;;  the Free Software Foundation, either version 3 of the License, or 
;;;  (at your option) any later version. 
;;;  
;;;  XBindJoy is distributed in the hope that it will be useful, 
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of 
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;;;  GNU General Public License for more details. 
;;; 
;;;  You should have received a copy of the GNU General Public License 
;;;  along with XBindJoy.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; module definition
(define-module (saulrh xbindjoy)
  #:export (
            ;; device information
            device->jsname
            jsname->device
            get-js-num-buttons
            get-js-num-axes
            ;; library functionality
            init-xbindjoy
            xbindjoy-start
            ;; xtest functions for sending events
            send-key
            send-keyseq
            send-mbutton
            send-mouserel
            send-mouseabs
            build-send-key-toggler
            text->keyseq
            ;; functions for binding things to buttons
            bind-button->proc
            bind-button->mbutton
            bind-button->key
            ;; functions for binding things to axes
            bind-axis->proc
            bind-axis-region->key
            ax-trans?
            ax-in-region?
            ax-threshold
            ))

;;; load up the library that provides the low-level stuff
(load-extension "libguilexbindjoy.so" "init_xbindjoy")


;;; 
(define-syntax-rule (bind-button->key buttoncode keycode)
  (begin (bind-button->proc (cons 'press buttoncode)
                            (lambda () (send-key 'press keycode 0)))
         (bind-button->proc (cons 'release buttoncode)
                            (lambda () (send-key 'release keycode 0)))))

(define-syntax-rule (bind-button->mbutton jsbutton mbutton)
  (begin (bind-button->proc (cons 'press jsbutton)
                            (lambda () (send-mbutton 'press mbutton 0)))
         (bind-button->proc (cons 'release jsbutton)
                            (lambda () (send-mbutton 'release mbutton 0)))))

(define (build-send-key-toggler key init)
  (let ((toggle-var init))
    (lambda ()
      (set! toggle-var (not toggle-var))
      (if toggle-var
          (send-key 'press key 0)
          (send-key 'release key 0)))))

(define (send-keyseq keylist)
  (let ((keys (car keylist)))
    (apply send-key keys)
    (if (not (null? (cdr keylist)))
        (send-keyseq (cdr keylist)))))

(define* (jsname->device name #:optional n)
  (if (not n)
      (jsname->device name 0)
      (let ((candidate (string-append "/dev/input/js" (number->string n))))
        (if (access? candidate R_OK)
            (if (equal? name (device->jsname candidate))
                candidate
                (jsname->device name (+ 1 n)))
            #f))))


(define (text->keyseq str)
  (string-fold-right (lambda (char next)
                       (let ((sym (string->symbol (string char))))
                         (cons (list 'press sym 0)
                               (cons (list 'release sym 0)
                                     next))))
                     '()
                     str))

(define* (ax-threshold axes deadzones)
  (map (lambda (ax)
         (let* ((idx (car ax))
                (val (cdr ax))
                (thr (assoc-ref deadzones idx))
                (a (car thr))
                (b (cdr thr))
                (lower (min a b))
                (upper (max a b)))
           (assoc-set! axes
                       idx
                       (if (and (< val upper)
                                (> val lower))
                           0.0
                           val))))
       axes))

(define (ax-trans? axes axes-last axis thresh toward-pos)
  (let ((cur (assoc-ref axes axis))
        (last (assoc-ref axes-last axis))
        (pred (if toward-pos > <)))
    (and (pred thresh last) (not (pred thresh cur)))))

(define (ax-in-region? axes axis a b)
  (let ((lower (min a b))
        (upper (max a b))
        (val (assoc-ref axes axis)))
    (and (> lower val) (< upper val))))

(define (bind-axis-region->key axis a b key)
  (let ((lower (min a b))
        (upper (max a b)))
    (bind-axis->proc (lambda (dt axes axes-last)
                       (if (ax-trans? axes axes-last axis lower #t) (send-key 'press key 0))
                       (if (ax-trans? axes axes-last axis lower #f) (send-key 'release key 0))
                       (if (ax-trans? axes axes-last axis upper #t) (send-key 'release key 0))
                       (if (ax-trans? axes axes-last axis upper #f) (send-key 'press key 0))))))
