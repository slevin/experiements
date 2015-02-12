#lang racket

(require racket/gui)
(require racket/draw)

(provide make-scene-context)

(struct scene-context (frame canvas [commands #:mutable]))

(define (make-scene-context width height)
  (let* ((fr (new frame%
                     [label "stuff"]
                     [width width]
                     [height height]))
        (cv (new canvas%
                      [paint-callback (lambda (canvas dc) #f)]
                      [parent fr]))
        (ctx (scene-context fr cv '())))
    (send fr show #t)
    ctx))

(define (render-scene 
(define (draw-circle context x y radius)
  (set-scene-context-commands! context
                               (cons
                                (lambda (dc)
                                  (let ((size (* radius 2)))
                                    (send dc draw-ellipse (- x radius) 
                                          (- y radius) size size)))
                                (scene-context-commands context))
                                ))

  