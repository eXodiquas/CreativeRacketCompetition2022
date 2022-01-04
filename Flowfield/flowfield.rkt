#lang racket

(require threading
         r-cade
         noise)

;; The linear algebra stuff gets moved to another file in the future.
;; TODO: Find out how documentation works in Racket. I think scribbl is the right thing to look into.
; Start LinAlg
(struct vec2d (x y) #:transparent)
(struct mat2x2 (a b c d) #:transparent)

(define (v+ v1 v2)
  (match-let ([(vec2d x1 y1) v1]
              [(vec2d x2 y2) v2])
    (vec2d (+ x1 x2) (+ y1 y2))))

(define (v* v s)
  (match-let ([(vec2d x y) v])
    (vec2d (* x s) (* y s))))

(define (matvec* m v)
  (match-let ([(mat2x2 a b c d) m]
              [(vec2d x y) v])
    (vec2d (+ (* a x) (* b y))
           (+ (* c x) (* d y)))))

(define (mat-compose left right)
  (match-let ([(mat2x2 a1 b1 c1 d1) left]
              [(mat2x2 a2 b2 c2 d2) right])
    (mat2x2 (+ (* a1 a2) (* b1 c2))
            (+ (* a1 b2) (* b1 d2))
            (+ (* a2 c1) (* c2 d1))
            (+ (* b2 c1) (* d1 d2)))))

(define (map-from-to x inp-start inp-end out-start out-end)
  (* (/ (- x inp-start) (- inp-end inp-start))
     (+ (- out-end out-start) out-start)))
; End LinAlg

(define *sw* 1000)
(define *sh* 1000)

(struct particle (m p f) #:transparent)

(define (apply-force p f dt)
  (match-let ([(particle m _ frc) p])
    (struct-copy particle p
                 [f (v+ frc (v* (v* f (/ 1 m)) dt))])))

(define (update-particle p)
  (match-let ([(particle m (vec2d x y) frc) p])
    (cond
      [(> x *sw*) (set! x 0)]
      [(<= x 0) (set! x *sw*)]
      [(> y *sh*) (set! y 0)]
      [(<= x 0) (set! y *sh*)])
    (particle m (v+ (vec2d x y) frc) (v* frc 0.95))))

(define (draw-particle p)
  (match-let ([(particle m (vec2d x y) _) p])
    (color (exact-floor (map-from-to m 0 1 0 15)))
    (circle x y 1 #:fill #t)
    p))

(define (create-vector-field width height #:noise (n 'simplex))
  (for/list ([x (range width)])
    (for/list ([y (range height)])
      (cond
        [(equal? n 'simplex) (vec2d (simplex (/ x *sw*)) (simplex (/ y *sh*)))]))))

(define (get-closest-vec vf p)
  (match-let ([(particle _ (vec2d x y) _) p])
    (list-ref (list-ref vf (modulo (exact-floor y) *sh*)) (modulo (exact-floor x) *sw*))))

(define *vf* (create-vector-field *sw* *sh*))

(define *ps* (for/list ([i (range 3000)])
               (particle (random) (vec2d (random *sw*) (random *sh*)) (vec2d 0 0))))

(define start-timer (timer 1))

(define (sketch)
  ;(cls) ; It also looks interesting if we clear the buffer each frame.
  (when (start-timer)
    (set! *ps* (map (λ (p)
                      (~> p
                          (apply-force (get-closest-vec *vf* p) (frametime))
                          update-particle)) *ps*))
    (for-each draw-particle *ps*)))

(run sketch
     *sw*
     *sh*)
