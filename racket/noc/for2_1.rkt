#lang racket

(require racket/draw)
(require racket/gui)
(require plot/utils)

(define mouse (vector 0 0))


(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (vector-set! mouse 0 (send event get-x))
      (vector-set! mouse 1 (send event get-y)))
    (super-new)))



(define (timer-tick)
  (define accel (v* (vnormalize (v- mouse pos)) 0.2))
  (set! vel (v+ vel accel))
  (set! pos (v+ pos vel))
  (send canvas refresh-now)
  )

(define timer (new timer%
                   [interval 16]
                   [notify-callback timer-tick]))


(class box%
  (init [width 640] [height 320])

  (define width width)
  (define height height)
  (define movers '())
  (super-new)

  (define/public (new-mover)
    ;; create mover in center and add to list of movers
    (let ((m (new mover%
                  [x (/ width 2)]
                  [y (/ height 2)])))
      (set! movers (cons m movers))))

  (define/public (get-width) width)
  (define/public (get-height) height)
  )

(class mover%
  (init [x 0] [y 0])

  (define pos (vector x y))
  (define vel (vector 0 0))
  (define acc (vector 0 0))

  (super-new))

(define bx (new box%))

(define frame (new frame%
                   [label "stuff"]
                   [width (send bx get-width)]
                   [height (send bx get-height)]))

(define canvas (new my-canvas%
                    [paint-callback painter]
                    [parent frame]))

(send frame show #t)


(send bx new-mover)


(define (painter canvas dc)
  (send dc set-pen "DarkRed" 3 'solid)
  (send dc set-brush "Firebrick" 'solid)
  (send dc draw-ellipse (- (vector-ref pos 0) 40) (- (vector-ref pos 1) 40) 80 80)
  )

;; how does it compare to their class?
;; is it worth making a class for somee of these functions?


;; gonna do the same thing but want to do something with gravity? fluids? some exercise in there?

;; figure out typed racket and possibly classes
