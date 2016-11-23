(use matchable doodle)

(world-inits
 (lambda ()
   (clear-screen)))

(world-changes
 (lambda (events dt exit)
   (for-each
    (lambda (e)
      (match e
       (('mouse 'pressed x y 1)
	(print "mouse pressed at xy=" x " " y))
       (('mouse 'released x y 1)
       	(print "mouse released at xy=" x " " y))
       (('mouse 'moved x y)
	(print "mouse moving at xy=" x " " y))
       (('key 'pressed #\esc)
        (exit #t))
       (else (void))))
    events)))

(new-doodle title: "Doodle" background: solid-white)
(run-event-loop)