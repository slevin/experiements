#lang typed/racket

(require racket/flonum)

(require/typed "for2extra.rkt"
               [gui-scene (-> Integer Integer Any)])

(struct: scene ([size : FlVector]))


(: make-scene (-> Flonum Flonum scene))
(define (make-scene width height)
  (gui-scene (fl->exact-integer width) (fl->exact-integer height))
  (scene (flvector width height))
  )

(make-scene 640.0 320.0)
