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
            send-button
            send-mouserel
            send-mouseabs
            lambda-send-key
            build-send-key-toggler
            ;; functions for working with keymaps
            define-key
            define-key-when
            define-shift
            bind-key
            build-axismap
            ;; functions for working with axes
            axesfun-with-history
            axis-transition
            axis-region
            ;; general utility functions
            normalize-jsaxes
            ;; variables
            last-axis-vals-alist
            cur-axis-vals-alist
            axis-dt
            axis-dt-nominal
            ;; the actual main loop
            xbindjoy-start))

;;; load up the library that provides the low-level stuff
(load-extension "../build/.libs/libxbindjoy.so" "init_xbindjoy")


;;; 
(define-syntax-rule (define-key keymap key func)
  (set! keymap (acons key func keymap)))

(define-syntax-rule (define-shift keymap key shift-var)
  (begin (define-key keymap (cons 'press key) (lambda () (set! shift-var #t)))
         (define-key keymap (cons 'release key) (lambda () (set! shift-var #f)))))

(define-syntax-rule (define-key-when keymap keycode preds exp* ...)
  (define-key keymap keycode (lambda () (when (every identity preds) exp* ...))))

(define-syntax-rule (lambda-send-key action key delay)
  (lambda () (send-key action key delay)))

(define-syntax-rule (bind-key keymap buttoncode keycode delay)
  (begin (define-key keymap (cons 'press buttoncode) (lambda () (send-key 'press keycode delay)))
         (define-key keymap (cons 'release buttoncode) (lambda () (send-key 'release keycode delay)))))

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







(define last-axis-vals-alist '())
(define cur-axis-vals-alist '())
(define axis-dt 0)
(define axis-dt-nominal (xbindjoy-get-axis-freq))

(define-syntax-rule (axesfun-with-history exp* ...)
  (lambda (dt axis-vals-alist)
    (set! cur-axis-vals-alist (normalize-jsaxes axis-vals-alist))
    (set! axis-dt dt)
    exp* ...
    (set! last-axis-vals-alist cur-axis-vals-alist)))

(define (normalize-jsaxes alist)
  (map
   (lambda (x) (cons (car x) (/ (cdr x) 32768)))
   alist))

(define (axis-transition axis thresh to+)
  (let ((cur (assoc-ref cur-axis-vals-alist axis))
        (last (assoc-ref last-axis-vals-alist axis))
        (pred (if to+ > <)))
    (and (pred thresh last) (not (pred thresh cur)))))

(define (axis-region axis a b key)
  (let ((lower (min a b))
        (upper (max a b)))
    (if (axis-transition axis lower #t) (send-key 'press key 0))
    (if (axis-transition axis lower #f) (send-key 'release key 0))
    (if (axis-transition axis upper #t) (send-key 'release key 0))
    (if (axis-transition axis upper #f) (send-key 'press key 0))))

