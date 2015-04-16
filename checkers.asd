(asdf:defsystem "checkers"
  :serial t
  :depends-on (:lispbuilder-sdl
               :anaphora
               :alexandria
               :optima
               :arrow-macros               
               )
  :components ((:file "checkers")))
                
