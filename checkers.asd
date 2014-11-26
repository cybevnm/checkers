(asdf:defsystem "checkers"
  :serial t
  :depends-on (:lispbuilder-sdl
               :anaphora
               :alexandria
               :optima
               
               )
  :components ((:file "checkers")))
                
