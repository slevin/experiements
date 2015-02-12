#lang typed/racket

(require racket/flonum)

(require/typed "for2extra.rkt"
               [scene-context (-> Integer Integer Any)])

(struct: scene ([size : FlVector]
                [context : Any]))


(: make-scene (-> Flonum Flonum scene))
(define (make-scene width height)
  (scene (flvector width height)
         (scene-context (fl->exact-integer width) (fl->exact-integer height))
         )
  )


(make-scene 640.0 320.0)


#|


|#