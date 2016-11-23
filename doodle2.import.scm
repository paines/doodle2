;;;; doodle2.import.scm - GENERATED BY CHICKEN 4.11.0 -*- Scheme -*-

(eval '(import
         chicken
         scheme
         srfi-1
         srfi-4
         srfi-18
         data-structures
         extras
         clojurian-syntax
         matchable
         (prefix sdl2-internals sdl2-internals:)
         (prefix sdl2 sdl2:)))
(##sys#register-compiled-module
  'doodle2
  (list)
  '((*sprites* . doodle2#*sprites*)
    (sprite-x . doodle2#sprite-x)
    (sprite-y . doodle2#sprite-y)
    (sprite-width . doodle2#sprite-width)
    (sprite-height . doodle2#sprite-height)
    (sprite-x-set! . doodle2#sprite-x-set!)
    (sprite-y-set! . doodle2#sprite-y-set!)
    (sprite-width-set! . doodle2#sprite-width-set!)
    (sprite-height-set! . doodle2#sprite-height-set!)
    (add-sprite! . doodle2#add-sprite!)
    (check-for-collisions . doodle2#check-for-collisions)
    (clear-screen . doodle2#clear-screen)
    (current-background . doodle2#current-background)
    (doodle2-width . doodle2#doodle2-width)
    (doodle2-height . doodle2#doodle2-height)
    (font-color . doodle2#font-color)
    (font-size . doodle2#font-size)
    (make-sprite . doodle2#make-sprite)
    (line-width . doodle2#line-width)
    (new-doodle2 . doodle2#new-doodle2)
    (remove-sprite! . doodle2#remove-sprite!)
    (run-event-loop . doodle2#run-event-loop)
    (show! . doodle2#show!)
    (solid-black . doodle2#solid-black)
    (solid-white . doodle2#solid-white)
    (update-sprite! . doodle2#update-sprite!)
    (world-changes . doodle2#world-changes)
    (world-inits . doodle2#world-inits)
    (world-ends . doodle2#world-ends)
    (world-update-delay . doodle2#world-update-delay))
  (list)
  (list))

;; END OF FILE