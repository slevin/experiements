#lang racket


(require racket/draw)
(require racket/gui)

(define width 640)
(define height 360)

(define x (/ width 2))
(define y (/ height 2))
(define xvel 0)
(define yvel 0)

(define frame (new frame%
                   [label "stuff"]
                   [width width]
                   [height height]))

(define (painter canvas dc)
  (send dc set-pen "DarkRed" 3 'solid)
  (send dc set-brush "Firebrick" 'cross-hatch)
  (send dc draw-ellipse x y 80 80)
  )

(define canvas (new canvas%
                    [paint-callback painter]
                    [parent frame]))

(send frame show #t)

;; mouse point
(define (timer-tick)
  (define dist 
  (define xa (
  (set! x (+ x xvel))
  (set! y (+ y yvel))
  (send canvas refresh-now)
  )

(define timer (new timer%
                   [interval 42]
                   [notify-callback timer-tick]))


;; accel my point - ball point / distance * some factor