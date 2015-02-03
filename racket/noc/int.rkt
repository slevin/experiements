#lang racket

(require racket/draw)
(require racket/gui)

(define frame (new frame%
                   [label "stuff"]
                   [width 640]
                   [height 360]))

(define x 320)
(define y 180)


(define cnv (new canvas% [parent frame]
                 [style '(no-autoclear)]
                 [paint-callback
                  (lambda (canvas dc)
                    (send dc draw-rectangle x y 2 2))]))

(send frame show #t)

(new timer%
     [interval 42]
     [notify-callback
      (lambda ()
        (let* ((rnd (random 4)))
          (cond 
            [(= rnd 0) (set! x (+ x 1))
                       (set! y (+ y 1))]
            [(= rnd 1) (set! x (+ x 1))
                       (set! y (- y 1))]
            [(= rnd 2) (set! x (- x 1))
                       (set! y (+ y 1))]
            [(= rnd 3) (set! x (- x 1))
                       (set! y (- y 1))]))
        (send cnv refresh-now)
        )])

