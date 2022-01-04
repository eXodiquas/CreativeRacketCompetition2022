#lang racket

(require r-cade)

(define *width* 1920)
(define *height* 1080)

(define *x-max*  5)
(define *x-min*  -5)
(define *y-max*  5)
(define *y-min*  -5)

(define (collatz x)
  (if (= (modulo x 2) 0)
      (/ x 2)
      (add1 (* 3 x))))

(define (collatz-sequence start)
  (define (aux c r)
    (if (= c 1)
        (cons 1 r)
        (aux (collatz c) (cons c r))))
  (reverse (aux start '())))

(define (x-coord->pixel x)
  (* (/ (- x *x-min*) (- *x-max* *x-min*))
     (+ (- *width* 0) 0)))

(define (y-coord->pixel y)
  (* (/ (- y *y-min*) (- *y-max* *y-min*))
     (+ (- *height* 0) 0)))

(define (coord->pixel x y)
  (values (exact-ceiling (x-coord->pixel x))
          (exact-ceiling (y-coord->pixel y))))

(define (make-same-length l1 l2)
  (let ([ll1 (length l1)]
        [ll2 (length l2)])
    (cond
      [(= ll1 ll2) (values l1 l2)]
      [(> ll1 ll2) (values l1 (append (for/list ([i (range (- ll1 ll2))]) 0) l2))]
      [(< ll1 ll2) (values (append (for/list ([i (range (- ll2 ll1))]) 0) l1) l2)])))

(define *p* '())

(define (sketch)
  (cls)
  (when (> (length *p*) 100)
    (set! *p* '()))
  (set! *p* (cons (list (random 1 1000) (random 1 1000)) *p*))
  (for ([p *p*])
    (match-let ([(list x y) p])
      (let ([xseq (collatz-sequence x)]
            [yseq (collatz-sequence y)])
        (let-values ([(xseqn yseqn) (make-same-length xseq yseq)])
          (for-each (λ (x y)
                      (let-values ([(xc1 yc1) (coord->pixel (- x) y)]
                                   [(xc2 yc2) (coord->pixel x (- y))]
                                   [(xc3 yc3) (coord->pixel (- x) (- y))]
                                   [(xc4 yc4) (coord->pixel x y)]
                                   [(zx zy) (coord->pixel 0 0)])
                        (color (modulo (* (length xseq) (length yseq)) 16))
                        (line xc1 yc1 zx zy)
                        (line xc2 yc2 zx zy)
                        (line xc3 yc3 zx zy)
                        (line xc4 yc4 zx zy))) xseqn yseqn))))))

(run sketch
     *width*
     *height*
     #:fps 5)
