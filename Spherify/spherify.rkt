#lang sketching

(require racket/list)

; Transforms (x, y) into (r, theta).
(define (cartesian->polar x y)
  (values (sqrt (+ (* x x) (* y y)))
          (if (= 0 x y)
              0
              (atan2 y x))))

; Transforms (r, theta) into (x, y).
(define (polar->cartesian r theta)
  (values (* r (cos theta))
          (* r (sin theta))))

(define (draw-split-line)
  (let ([wh (/ width 2)])
    (line wh 0 wh height)))

(define (draw-rect x y w h)
  (let-values ([(xx yy) (cartesian->polar x y)])
    (let ([tx (remap xx 0 1000 400 800)]
          [ty (remap yy 0 (/ pi 2) 0 400)])
      (fill 255)
      (rect x y w h)
      (rect tx ty w h))))

(define (draw-origin)
  (let-values ([(xx yy) (cartesian->polar 0 0)])
    (let ([tx (remap xx 0 1000 400 800)]
          [ty (remap yy 0 (/ pi 2) 0 400)])
      (fill 255 0 0 255)
      (rect 0 0 5 5)
      (rect tx ty 5 5))))

(define points '())

(define (setup)
  (size 800 400)
  (frame-rate 60))

(define (draw)
  (background 255)
  (draw-split-line)
  (draw-origin)
  (when mouse-pressed
    (:= points (cons (list mouse-x mouse-y) points)))
  (for ([p points])
    (draw-rect (first p) (second p) 2 2)))
