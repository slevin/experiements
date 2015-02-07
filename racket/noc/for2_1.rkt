#lang racket

(require racket/draw)
(require racket/gui)
(require plot/utils)

(define my-canvas%
  (class canvas%
    (define mouse-pos (vector 0 0))

    (define/override (on-event event)
      (vector-set! mouse-pos 0 (send event get-x))
      (vector-set! mouse-pos 1 (send event get-y)))

    (define/public (get-mouse) mouse-pos)
    (super-new)))


(define mover%
  (class object%
    (init [x 0] [y 0])

    (define pos (vector x y))
    (define vel (vector 0 0))
    (define acc (vector 0 0))

    (define/public (apply-force v)
      (set! acc  (v+ acc v)))

    (define/public (update)
      (set! vel (v+ vel acc))
      (set! acc (vector 0 0))
      (set! pos (v+ pos vel))
      )
    (define/public (get-pos) pos)

    (define/public (paint-me dc)
      (send dc draw-ellipse
            (- (vector-ref pos 0) 40)
            (- (vector-ref pos 1) 40) 80 80))

    (super-new)))

(define box%
  (class object%
    (init [width 640] [height 320])

    (define _width width)
    (define _height height)
    (define movers '())
    (define frame #f)
    (define canvas #f)
    (define timer #f)

    (super-new)

    (define/public (new-mover)
      ;; create mover in center and add to list of movers
      (let ((m (new mover%
                    [x (/ _width 2)]
                    [y (/ _height 2)])))
        (set! movers (cons m movers))))

    (define/public (update)
      (for ([m movers])
        (let ((mouse-accel (v* (vnormalize (v- (send canvas get-mouse) (send m get-pos))) 0.2)))
          (send m apply-force mouse-accel)
          (send m update)))
      (send canvas refresh-now))

    (define (paint-me dc)
      (send dc set-pen "DarkRed" 3 'solid)
      (send dc set-brush "Firebrick" 'solid)
      (for ([m movers]) (send m paint-me dc)))

    (define/public (show-scene)
      (set! frame (new frame%
                       [label "stuff"]
                       [width _width]
                       [height _height]))

      (set! canvas (new my-canvas%
                        [paint-callback (lambda (canvas dc) (paint-me dc))]
                        [parent frame]))
      (send frame show #t)
      (set! timer (new timer%
                       [interval 16]
                       [notify-callback (lambda () (update))])))

    ))



(define bx (new box%))

(send bx new-mover)

(send bx show-scene)

;; gonna do the same thing but want to do something with gravity? fluids? some exercise in there?

;; figure out typed racket and possibly classes
