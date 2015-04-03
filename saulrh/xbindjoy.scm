;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; module definition
(define-module (saulrh xbindjoy)
  #:export (
            ;; device name handling
            device->jsname
            jsname->device
            ;; xtest functions for sending events
            send-key
            send-keyseq
            send-mousebutton
            send-mouserel
            send-mouseabs
            text->keyseq
            build-send-key-toggler
            get-js-num-buttons
            get-js-num-axes
            ;; functions for working with bindings
            init-xbindjoy
            bind-button
            bind-button-when
            bind-button-to-button
            bind-key-to-button
            define-shift
            build-axismap
            ;; functions for working with axes
            bind-axis
            bind-key-to-axis-region
            ax-trans?
            ax-in-region?
            ;; general utility functions
            normalize-jsaxes
            ;; variables
            last-axis-vals-alist
            cur-axis-vals-alist
            ;; the actual main loop
            xbindjoy-start))

;;; load up the library that provides the low-level stuff
(load-extension "libguilexbindjoy.so" "init_xbindjoy")


;;; 
(define-syntax-rule (define-shift key shift-var)
  (begin (bind-button (cons 'press key) (lambda () (set! shift-var #t)))
         (bind-button (cons 'release key) (lambda () (set! shift-var #f)))))

(define-syntax-rule (bind-button-when keycode preds exp* ...)
  (bind-button keycode (lambda () (when (every identity preds) exp* ...))))

(define-syntax-rule (bind-key-to-button buttoncode keycode)
  (begin (bind-button (cons 'press buttoncode) (lambda () (send-key 'press keycode 0)))
         (bind-button (cons 'release buttoncode) (lambda () (send-key 'release keycode 0)))))

(define-syntax-rule (bind-button-to-button jsbutton mousebutton)
  (begin (bind-button (cons 'press jsbutton) (lambda () (send-mousebutton 'press mousebutton 0)))
         (bind-button (cons 'release jsbutton) (lambda () (send-mousebutton 'release mousebutton 0)))))

(define (build-send-key-toggler k init)
  (let ((toggle-var init))
    (lambda ()
      (set! toggle-var (not toggle-var))
      (if toggle-var
          (send-key 'press k 0)
          (send-key 'release k 0)))))

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
                         (cons `(press ,sym 0)
                               (cons `(release ,sym 0)
                                     next))))
                     '()
                     str))





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

(define (bind-key-to-axis-region axis a b key)
  (let ((lower (min a b))
        (upper (max a b)))
    (bind-axis (lambda (dt axes axes-last)
                 (if (ax-trans? axes axes-last axis lower #t) (send-key 'press key 0))
                 (if (ax-trans? axes axes-last axis lower #f) (send-key 'release key 0))
                 (if (ax-trans? axes axes-last axis upper #t) (send-key 'release key 0))
                 (if (ax-trans? axes axes-last axis upper #f) (send-key 'press key 0))))))
