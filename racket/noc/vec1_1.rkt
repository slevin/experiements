#lang racket


(require racket/draw)
(require racket/gui)
(require plot/utils)

(define width 640)
(define height 360)

(define pos (vector (/ width 2) (/ height 2)))
(define vel (vector 0 0))
(define mouse (vector 0 0))

(define frame (new frame%
                   [label "stuff"]
                   [width width]
                   [height height]))

(define (painter canvas dc)
  (send dc set-pen "DarkRed" 3 'solid)
  (send dc set-brush "Firebrick" 'solid)
  (send dc draw-ellipse (- (vector-ref pos 0) 40) (- (vector-ref pos 1) 40) 80 80)
  )

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (vector-set! mouse 0 (send event get-x))
      (vector-set! mouse 1 (send event get-y)))
    (super-new)))

(define canvas (new my-canvas%
                    [paint-callback painter]
                    [parent frame]))

(send frame show #t)

      
(define (timer-tick)
  (define accel (v* (vnormalize (v- mouse pos)) 0.2))
  (set! vel (v+ vel accel))
  (set! pos (v+ pos vel))
  (send canvas refresh-now)
  )

(define timer (new timer%
                   [interval 16]
                   [notify-callback timer-tick]))


;; how does it compare to their class?
;; is it worth making a class for somee of these functions?

