#lang racket

(require racket/gui)
(require racket/draw)

(provide gui-scene)

(define (gui-scene width height)
  (define frame (new frame%
                     [label "stuff"]
                     [width width]
                     [height height]))
  
  (define canvas (new canvas%
                      [paint-callback (lambda (canvas dc) #f)]
                      [parent frame]))
  
  (send frame show #t)
  #f
  )

