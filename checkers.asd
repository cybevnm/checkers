(asdf:defsystem "checkers"
  :serial t
  :depends-on (:lispbuilder-sdl
               :anaphora
               :alexandria
               :optima
               :arrow-macros
               :lisp-unit2
               :cl-dot ;; to draw ai trees
               )
  :components ((:file "checkers")))
                
