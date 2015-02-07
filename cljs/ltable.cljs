(ns slevin.lighttable
  (:require lt.object)
  (:require-macros lt.macros))

(lt.object/object* ::mytemp
                   :tags [:mytag]
                   :somestate []
                   :otherstate {})

(lt.object/->def ::mytemp)



(lt.object/create ::mytemp)



(lt.macros/behavior ::mybehave
                    :triggers #{:mytrig}
                    :reaction (fn [self & rest]
                                (prn (str "self:" self " and: " rest))))

(lt.object/->behavior ::mybehave)

(lt.object/add-behavior!
 (first (lt.object/by-tag :mytag))
 [::mybehave])

(lt.object/raise
 (first (lt.object/by-tag :mytag))
 :mytrig)

(lt.object/raise
 (first (lt.object/by-tag :mytag))
 :mytrig
 "some"
 "other"
 "stuff")

