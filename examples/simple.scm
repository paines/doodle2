(use matchable doodle2 doodle2-colors)

(define *background-mode* #t)
(define *time* (/ 1 60))

(world-inits
 (lambda ()
   (clear-screen solid-white)))

(world-changes
 (lambda (events dt exit)
   (for-each
    (lambda (e)
      (match e
       (('mouse 'pressed x y 1)
	(print "mouse pressed at x=" x " " "y=" y))
       (('mouse 'released x y 1)
       	(print "mouse released at x=" x " " "y=" y))
       (('mouse 'moved x y)
	(print "mouse moving at x=" x " " "y=" y))
       (('mousebutton 'pressed b)
	(print "mouse button pressed=" b))
       (('mousebutton 'released b)
	(print "mouse button released=" b))
       (('key 'pressed #\esc)
        (exit #t))
       (else (void))))
    events)

   (clear-screen deeppink)
   (draw-circle 0 0 0.5 100 solid-black)
   (update-screen)
   ))   

(new-doodle2 title: "Doodle2" background: solid-white)
;(run-event-loop)
(run-event-loop run-in-background: *background-mode* minimum-wait: *time*)
