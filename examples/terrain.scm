(use matchable doodle2 doodle2-colors gl-math defstruct gl gl-utils)

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


(define projection-matrix (perspective 800 600 -1 1 90))

(define view-matrix
  (look-at (make-point 1 0 3)
           (make-point 0 0 0)
           (make-point 0 1 0)))

(define model-matrix (mat4-identity))

(defstruct point2d
  x
  y)

;;http://webglfactory.blogspot.de/2011/05/how-to-convert-world-to-screen.html
(define (world2screen p vm pm w h)
  (let* ((vpm (m* pm vm))
         (p3d (m* vpm p))
         (winX (round (* (/ (+ (f32vector-ref p3d 0) 1) 2) w)))
         (winY (round (* (/ (- 1 (f32vector-ref p3d 1)) 2) h)))
         (p2d (make-point2d x: winX y: winY)))
    p2d))

(define (screen2world p2d pm vm w h)
  (let* (
         (x (- (/ (* 2 (point2d-x p2d)) w) 1))
         (y (+ (/ (* -2 (point2d-y p2d)) h) 1))
         (vpi (inverse pm vm))
         (p3d (make-point x y 0)))
    (m* vpi p3d)))


(define scl 0.5)
(define (draw-terrain)
  (gl:Rotatef (/ pi `3) 0 0 -1)
                                        ;
                                        ;  (gl:Rotatef pi -1 1 -1)

  (gl:PolygonMode gl:FRONT_AND_BACK gl:LINE)
  (set-color chocolate)
  (do ((y -3.5  (+ y scl)))
      ((> y 3.5))
    (gl:Begin gl:TRIANGLE_STRIP)

    (do ((x -1.5 (+ x scl)))
        ((> x 7.5))
     
      (gl:Vertex3f (* x scl) (* y scl ) (noise2d x y))
      (gl:Vertex3f (* x scl) (* (+ y 1) scl) (noise2d x (+ y 1))))
    (gl:End)))

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

   (clear-screen solid-black)
   (draw-terrain)
   (update-screen)
   ))   

(new-doodle2 title: "Doodle2" background: solid-white width: 600 height: 600)
;(run-event-loop)
(run-event-loop run-in-background: *background-mode* minimum-wait: *time*)




