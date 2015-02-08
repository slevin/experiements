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
    (init [x 0] [y 0] [mass 10])

    (define pos (vector x y))
    (define vel (vector 0 0))
    (define acc (vector 0 0))
    (define _mass mass)
    
    (define/public (apply-force v)
      (set! acc  (v+ acc (v/ v _mass))))

    (define/public (update)
      (set! vel (v+ vel acc))
      (set! acc (vector 0 0))
      (set! pos (v+ pos vel))
      )
    (define/public (get-pos) pos)

    (define (get-radius) (* _mass 4))
    
    (define (bounce-it pos-index max)
      (if (or (and (> 0 (- (vector-ref pos pos-index) (get-radius)))
                   (> 0 (vector-ref vel pos-index))
                   )
              (and (< max (+ (vector-ref pos pos-index) (get-radius)))
                   (< 0 (vector-ref vel pos-index))
                   ))
          (vector-set! vel pos-index (* -1 (vector-ref vel pos-index)))
          #f))
    
    (define/public (enforce-bounds max-x max-y)
      (bounce-it 0 max-x)
      (bounce-it 1 max-y))

    (define/public (paint-me dc)
      (let* ((radius (get-radius))
             (size (* radius 2)))
        (send dc draw-ellipse
              (- (vector-ref pos 0) radius)
              (- (vector-ref pos 1) radius) size size)))

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

    (define/public (new-mover mass)
      ;; create mover in center and add to list of movers
      (let ((m (new mover%
                    [x (/ _width 2)]
                    [y (/ _height 2)]
                    [mass mass])))
        (set! movers (cons m movers))))

    (define/public (update)
      (for ([m movers])
        (let (
              ;;(mouse-accel (v* (vnormalize (v- (send canvas get-mouse) (send m get-pos))) 0.2))
              )
          ;;(send m apply-force mouse-accel)
          (send m apply-force (vector 0.1 3))
          (send m update)
          (send m enforce-bounds _width _height)))
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

(send bx new-mover 5)

(send bx new-mover 10)
(send bx new-mover 2)
(send bx new-mover 14)
(send bx show-scene)
