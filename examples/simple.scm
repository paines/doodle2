(use matchable doodle2 doodle2-colors)

(define *background-mode* #t)
(define *time* (/ 1 60))

(define *screenshot* "doodl2-screenshot-")

(define (draw-spin tick col)
  
  (let* ((t tick)
	 (steps 100)
	 (step (/ 1 steps)))
    
    (do((i 0 (add1 i)))
	((> i steps))	
      (let* ((r (* step i))
  	       (r2 (+ r step)))
	(draw-line (* r (cos (* t r)))
		   (* r (sin (* t r)))
		   (* r2 (cos (* t r2)))
		   (* r2 (sin (* t r2))) col 0.2)))))

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
        (('key 'pressed 'left)
         (pp "key left pressed"))
        (('key 'pressed 'right)
         (pp "key right pressed"))
        (('key 'pressed 'down)
         (save-screenshot "screenshot")
         )
        
        (('key 'pressed 'up)
         (pp "key up pressed"))
        (('key 'pressed 'escape)
         (exit #t))
        (else (void))))
    events)

   (clear-screen deeppink)
   (draw-line 0.0 0.0 0.5 0.5 solid-white 0.2)
   (draw-circle 0 0 0.3 100 solid-white)
   (draw-spin (doodle2-ticks) solid-white)
   (rectangle 0 0 0.5 0.5 solid-white 0.2 filled: #t)
   (update-screen)
   ))   

(new-doodle2 title: "Doodle2" background: solid-white)
;(run-event-loop)
(run-event-loop run-in-background: *background-mode* minimum-wait: *time*)
