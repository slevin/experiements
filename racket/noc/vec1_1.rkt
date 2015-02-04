#lang racket


(require racket/draw)
(require racket/gui)

(define width 640)
(define height 360)

(define x (/ width 2))
(define y (/ height 2))
(define xvel 0)
(define yvel 0)
(define mx 0)
(define my 0)

(define frame (new frame%
                   [label "stuff"]
                   [width width]
                   [height height]))

(define (painter canvas dc)
  (send dc set-pen "DarkRed" 3 'solid)
  (send dc set-brush "Firebrick" 'cross-hatch)
  (send dc draw-ellipse x y 80 80)
  )

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (set! mx (send event get-x))
      (set! my (send event get-y)))
    (super-new)))

(define canvas (new my-canvas%
                    [paint-callback painter]
                    [parent frame]))

(send frame show #t)

      
(define (timer-tick)
  (define dist (sqrt (+ (expt (- mx x) 2) (expt (- my y) 2))))
  (define xa (* (/ (- mx x) dist) 0.5))
  (define ya (* (/ (- my y) dist) 0.5))
  (set! xvel (+ xvel xa))
  (set! yvel (+ yvel ya))
  (set! x (+ x xvel))
  (set! y (+ y yvel))
  (send canvas refresh-now)
  )

(define timer (new timer%
                   [interval 42]
                   [notify-callback timer-tick]))


;; should draw at center, not at corner
;; why so slow, how can I make it faster (I need it to be faster)
;; how does it compare to what I did before
