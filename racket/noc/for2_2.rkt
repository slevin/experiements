#lang typed/racket

(require racket/flonum)

(require/typed "for2extra.rkt"
               [#:opaque Scene-context scene-context?]
               [make-scene-context (-> Integer Integer Scene-context)]
               [draw-circle (-> Scene-context Flonum Flonum Flonum Void)]
               [render-scene (-> Scene-context Void)]
               [start-updates (-> Scene-context (-> Void) Void)]
               [clear-scene (-> Scene-context Void)])

(struct: mover ([position : FlVector]
                [velocity : FlVector]
                [radius : Flonum]))

(struct: scene ([size : FlVector]
                [movers : (Listof mover)]
                [context : Scene-context]) #:mutable)


(: make-scene (-> Flonum Flonum scene))
(define (make-scene width height)
  (scene (flvector width height)
         '()
         (make-scene-context (fl->exact-integer width) (fl->exact-integer height))
         )
  )

(: add-mover (-> scene Flonum Flonum Flonum Void))
(define (add-mover sc pos-x pos-y radius)
  (set-scene-movers! sc (cons (mover (flvector pos-x pos-y) (flvector 0.0 0.0) radius)
                           (scene-movers sc))))

(: draw-mover (-> Scene-context mover Void))
(define (draw-mover ctx m)
  (draw-circle ctx
               (flvector-ref (mover-position m) 0)
               (flvector-ref (mover-position m) 1)
               (mover-radius m)))

   
(define sc (make-scene 640.0 320.0))
(add-mover sc 40.0 40.0 40.0)
(add-mover sc 100.0 100.0 60.0)

(start-updates (scene-context sc)
               (lambda ()
                 (clear-scene (scene-context sc))
                 (for ([m (scene-movers sc)])
                   (draw-mover (scene-context sc) m))
                 (render-scene (scene-context sc))
                 ))

#|

would like to be able to add a force
maybe should send a make scene context that triggers updates to things
based on a set of forces


probably need drawing into buffered commands

need some clear commands

then swapping in the buffered and saying refresh now


|#