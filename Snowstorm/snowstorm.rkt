#lang sketching

(require threading
         racket/match
         racket/list
         racket/math
         (file "~/Repositories/Sapientia/sapientia.rkt"))

(struct snow-flake (position velocity acceleration size color))

(define (draw-snow-flake sf)
  (match-let ([(snow-flake (vec2d x y) _ _ s c) sf])
    (fill c)
    (ellipse x y s s)))

(define (generate-snow-flake)
  (snow-flake (vec2d (random 0 width) (random 0 -250))
              (vec2d 0 0)
              (vec2d (random -0.1 0.1) (random 0.01 0.3))
              (random 10 15)
              (color (random 255) (random 255) (random 255) 255)))

(define (update-snow-flake sf)
  (match-let ([(snow-flake p v a _ _) sf])
    (:= sf.position (λv-y (λ (y) (min height y))
                          (λv-x (λ (x)
                                  (modulo (exact-floor x) width))
                                (v+ p v))))
    (:= sf.velocity (v+ v a))))

(define (apply-force-snow-flake sf frc)
  (match-let ([(snow-flake (vec2d x y) _ _ _ _) sf])
    (when (> y (- height 50))
      (:= sf.acceleration frc))))

(define (apply-gravity-snow-flake sf)
  (match-let ([(snow-flake (vec2d x y) _ _ _ _) sf])
    (:= sf.acceleration (v+ sf.acceleration (vec2d 0 0.01)))))

(define snow-flakes '())

(define (setup)
  (size 800 640)
  (frame-rate 60)
  (no-stroke)
  (:= snow-flakes (for/list ([i (range 0 500)])
                    (generate-snow-flake))))

(define (draw)
  ;(background 0)
  (for ([sf snow-flakes])
    (update-snow-flake sf)
    (draw-snow-flake sf)
    (apply-gravity-snow-flake sf)
    (apply-force-snow-flake sf (vec2d (random -0.1 0.1) (random -0.3 -0.2)))))
