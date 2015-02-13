#lang racket

(require racket/gui)
(require racket/draw)

(provide make-scene-context
         render-scene
         draw-circle
         scene-context?)

(struct scene-context (frame canvas [commands #:mutable]))

(define (make-scene-context width height)
  (let* ((fr (new frame%
                     [label "stuff"]
                     [width width]
                     [height height]))
        (cv (new canvas% [parent fr]))
        (ctx (scene-context fr cv '())))
    (send fr show #t)
    ctx))


(define (render-scene context)
  (send (scene-context-canvas context)
        refresh-now
        (lambda (dc)
          (for ([c (scene-context-commands context)])
            (c dc)))))

(define (add-draw-command context command)
  (set-scene-context-commands! context 
                               (cons command 
                                     (scene-context-commands context))))

(define (draw-circle context x y radius)
  (add-draw-command context
                    (lambda (dc)
                      (let ((size (* radius 2))
                            (new-x (- x radius))
                            (new-y (- y radius)))
                        (send dc draw-ellipse new-x new-y size size)))))

#|

set colors

some notion of a timer that's firing off each second


|#