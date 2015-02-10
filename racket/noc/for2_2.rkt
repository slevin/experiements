#lang typed/racket

(require racket/flonum)

(require/typed "for2extra.rkt"
               [gui-scene (-> Flonum Flonum Any)])

(struct: scene ([size : FlVector]))


(: make-scene (-> Flonum Flonum scene))
(define (make-scene width height)
  (gui-scene width height)
  (scene (flvector width height))
  )
