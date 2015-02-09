#lang typed/racket

(require racket/gui)
(require racket/draw)
(require racket/flonum)

(struct: scene ([size : FlVector]))


(: make-scene (-> Flonum Flonum scene))
(define (make-scene width height)
  (define frame (new frame%
                     [label "stuff"]
                     [width width]
                     [height height]))
  (scene (flvector width height))
  )
