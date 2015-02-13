#lang typed/racket

(require racket/flonum)

(require/typed "for2extra.rkt"
               [#:opaque Scene-context scene-context?]
               [make-scene-context (-> Integer Integer Scene-context)]
               [draw-circle (-> Scene-context Flonum Flonum Flonum Void)]
               [render-scene (-> Scene-context Void)])

(struct: scene ([size : FlVector]
                [context : Scene-context]))


(: make-scene (-> Flonum Flonum scene))
(define (make-scene width height)
  (scene (flvector width height)
         (make-scene-context (fl->exact-integer width) (fl->exact-integer height))
         )
  )


(define sc (make-scene 640.0 320.0))
(draw-circle (scene-context sc) 40. 40. 40.)
(draw-circle (scene-context sc) 40. 40. 60.)
(render-scene (scene-context sc))

#|


|#