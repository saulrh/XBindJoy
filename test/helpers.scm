;;; If I can find a sane way to handle initscripts for an extended
;;; guile interpreter, these should be moved to a global initscript,
;;; kind of like how shells have /etc/profile.
(define-syntax define-key
  (syntax-rules ()
    ((define-key keymap key func)
     (set! keymap (acons key func keymap)))))

(define (send-keyseq keylist)
  (let ((keys (car keylist)))
    (apply send-key keys)
    (if (not (null? (cdr keylist)))
        (send-keyseq (cdr keylist)))))

(define-syntax bind-key
  (syntax-rules ()
    ((bind-key keymap buttoncode keycode delay)
     (begin (define-key keymap `(press . ,buttoncode) (lambda () (send-key 'press keycode delay)))
            (define-key keymap `(release . ,buttoncode) (lambda () (send-key 'release keycode delay)))))))

(define* (jsname->device name #:optional n)
  (if (not n)
      (jsname->device name 0)
      (let ((candidate (string-append "/dev/input/js" (number->string n))))
        (if (access? candidate R_OK)
            (if (equal? name (device->jsname candidate))
                candidate
                (jsname->device name (+ 1 n)))
            #f))))

(define (scale-vals-alist alist)
  (map
   (lambda (x) (cons (car x) (/ (cdr x) 32768)))
   alist))

(define-syntax define-shift
  (syntax-rules ()
    ((define-shift keymap key shift-var)
     (begin (define-key keymap `(press . ,key) (lambda () (set! shift-var #t)))
            (define-key keymap `(release . ,key) (lambda () (set! shift-var #f)))))))
