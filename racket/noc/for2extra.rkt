#lang racket

(require racket/gui)
(require racket/draw)

(provide make-scene-context
         render-scene
         draw-circle
         scene-context?)

(struct scene-context (frame canvas [commands #:mutable] timer))

(define command-canvas%
  (class canvas%
    (define commands '())

    (define/public (add-command command)
      (set! commands (cons command commands)))
    
    (define/public (get-commands)
      commands)
    
    (super-new)))

(define (make-scene-context width height on-timer-tick)
  (let* ((fr (new frame%
                     [label "stuff"]
                     [width width]
                     [height height]))
        (cv (new command-canvas% 
                 [parent fr]
                 [paint-callback (lambda (canvas dc) (render-commands dc (send canvas get-commands)))]))
        (tm (new timer%
                 [interval 16]
                 [notify-callback on-timer-tick]))
        (ctx (scene-context fr cv '() tm)))
    (send fr show #t)
    ctx))


(define (render-commands dc commands)
  (for ([c commands])
    (c dc)))

(define (render-scene context)
  (send (scene-context-canvas context) refresh-now))

(define (add-draw-command context command)
  (send (scene-context-canvas context) add-command command))

(define (draw-circle context x y radius)
  (add-draw-command context
                    (lambda (dc)
                      (send dc set-pen "DarkRed" 3 'solid)
                      (send dc set-brush "Firebrick" 'solid)
                      (let ((size (* radius 2))
                            (new-x (- x radius))
                            (new-y (- y radius)))
                        (send dc draw-ellipse new-x new-y size size)))))

#|
why erase

some notion of a timer that's firing off each second


|#