#lang racket

(require racket/draw)
(require racket/gui)

(define frame (new frame%
                   [label "stuff"]
                   [width 640]
                   [height 360]))

(define x 320)
(define y 180)

(new canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc draw-rectangle x y 2 2))])

(send frame show #t)

;; canvas has an animation callback that I might draw in


;; draw dot

;; then do random walk

;; need run loop on a timer
