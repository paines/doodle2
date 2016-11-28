;; Doodle2 - Drawing and gaming made easy
;;
;; Copyright (c) 2016, Anes Lihovac (adapted to SDL2)
;; Copyright (c) 2012, Christian Kellermann
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;;     Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;     Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials provided
;;     with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(module doodle2
    (*sprites*
     sprite-x
     sprite-y
     sprite-width
     sprite-height
     sprite-x-set!
     sprite-y-set!
     sprite-width-set!
     sprite-height-set!
     add-sprite!
;     blit-image
     check-for-collisions
     clear-screen
     update-screen
     current-background
     doodle2-width
     doodle2-height
;     define-resource
     font-color
     font-size
     make-sprite
     line-width
     new-doodle2
     remove-sprite!
     run-event-loop
     save-screenshot
     show!
     solid-black
     solid-white
;     textxb
     update-sprite!
     world-changes
     world-inits
     world-ends
     world-update-delay
     doodle2-ticks
     draw-circle
     draw-line
     rectangle)


  (import chicken scheme)
  (use (srfi 1 4 18) data-structures extras clojurian-syntax matchable gl glu gl-utils gl-math defstruct lolevel bind)

  (use (prefix sdl2-internals sdl2-internals:))
  (use (prefix sdl2 sdl2:))

  (include "helpers.scm")

  (define projection-matrix #f)

  (define view-matrix
    (look-at (make-point 1 0 3)
  	     (make-point 0 0 0)
  	     (make-point 0 1 0)))
  
  (define model-matrix (mat4-identity))

  (defstruct point2d
  x
  y)
   
  (define *font-color* '(1 1 1 1))
  (define (font-color . c)
    (if (null? c)
	*font-color*
	(set! *font-color* (car c))))
  (define *font-size* 12)
  (define (font-size . s)
    (if (null? s)
	*font-size*
	(set! *font-size* (car s))))

  (define *line-width* 2.0)
  (define (line-width . w)
    (if (null? w)
	*line-width*
	(set! *line-width* (car w))))

  (define *current-background* '(0 0 0 1))
  (define (current-background . c)
    (if (null? c)
	*current-background*
	(set! *current-background* (car c))))

  (define solid-black (list 0 0 0 1))
  (define solid-white (list 1 1 1 0))

  (define *w* #f)

  (define doodle2-width #f)
  (define doodle2-height #f)


  (define *world-inits* values)
  (define *world-changes* values)
  (define *world-ends* values)
  (define *minimum-wait* 0)

  (define (world-ends f) (set! *world-ends* f))
  (define (world-changes f) (set! *world-changes* f))
  (define (world-inits f) (set! *world-inits* f))

  (define (world-update-delay d)
    (unless (number? d)
      (error "Please provide a number for the world-update-delay, given " d))
    (set! *minimum-wait* d))

  (define (save-screenshot filename)
    
    (define vector (make-vector (* doodle2-width doodle2-height 4)))
    (gl:ReadPixels 0 0 doodle2-width doodle2-height gl:RGBA gl:UNSIGNED_BYTE (object->pointer vector))

    (sdl2:save-bmp! (sdl2:create-rgb-surface-from* (object->pointer vector) doodle2-width doodle2-height 32 (* 4 doodle2-width) 0 0 0 0) (string-append filename "-" (get-timestamp) ".bmp"))
    
    )
    

  (define (new-doodle2 #!key
		      (width 800)
		      (height 600)
		      (title "Doodle2")
		      (color solid-black)
		      (fullscreen #f))

    (sdl2:set-main-ready!)
    (sdl2:init! '(video))
    (set! *w* (sdl2:create-window! title 0 0 width height '(opengl)))
    (sdl2:gl-create-context! *w*)
    (set! (sdl2:window-title *w*) title)

    (set! doodle2-width width)
    (set! doodle2-height height)

    (clear-screen color)

    (gl:Viewport 0 0 doodle2-width doodle2-height)
    (gl:MatrixMode gl:PROJECTION)
    (gl:LoadIdentity)
    (gl:ClearColor 1 0 0 0)
    
    (glu:Ortho2D -1 1 -1 1)
    (gl:MatrixMode gl:MODELVIEW)
    (gl:LoadIdentity)
    
;    (set!  projection-matrix (perspective doodle2-width doodle2-height -1 1 90))
    
    ;; (gl:Disable gl:TEXTURE_2D)
    ;; (gl:Disable gl:DEPTH_TEST)
    ;; (gl:Disable gl:COLOR_MATERIAL)
    ;; (gl:Enable gl:BLEND)
    ;; (gl:Enable gl:POLYGON_SMOOTH)
    ;; (gl:BlendFunc gl:SRC_ALPHA gl:ONE_MINUS_SRC_ALPHA)
    ;; (gl:Enable gl:POINT_SMOOTH)
    
    (sdl2:window-surface *w*)
    (sdl2:update-window-surface! *w*))

    ;;http://webglfactory.blogspot.de/2011/05/how-to-convert-world-to-screen.html
  (define (world2screen p vm pm w h)
    (let* ((vpm (m* pm vm))
  	   (p3d (m* vpm p))
  	   (winX (round (* (/ (+ (f32vector-ref p3d 0) 1) 2) w)))
  	   (winY (round (* (/ (- 1 (f32vector-ref p3d 1)) 2) h)))
  	   (p2d (make-point2d x: winX y: winY)))
      p2d))

  (define (screen2world p2d)
    (let* (
  	   (x (- (/ (* 2 (point2d-x p2d)) doodle2-width) 1))
  	   (y (+ (/ (* -2 (point2d-y p2d)) doodle2-height) 1))
  	   (vpi (inverse projection-matrix view-matrix))
  	   (p3d (make-point x y 0)))
      (m* vpi p3d)))


  (define (update-screen)
    (gl:Flush)
    (sdl2:gl-swap-window! *w*))
  
  (define (clear-screen color)
    (gl:ClearColor (car color)
		   (car (cdr color))
		   (car (cddr color))
		   (car (cdddr color)))

    (gl:Clear (+ gl:COLOR_BUFFER_BIT gl:DEPTH_BUFFER_BIT)))
	   
  (define (show!)
    (sdl2:show-window! *w*))


  ;; Collision detection

  ;; sprites are very crude rectangles atm consisting of a name, x, y,
  ;; width and height in pixels
  (define-record sprite  name x y width height)
  (define *sprites* '())

  (define-record-printer (sprite s p)
    (fprintf p "#,(sprite ~s ~s ~s ~s ~s)"
	     (sprite-name s)
	     (sprite-x s)
	     (sprite-y s)
	     (sprite-width s)
	     (sprite-height s)))

  (define (collides? s ss)
    (define (inside? x y s)
      (and (< (sprite-x s) x (+ (sprite-x s) (sprite-width s)))
	   (< (sprite-y s) y (+ (sprite-y s) (sprite-height s)))))

    (define (sprite->points s)
      `((,(sprite-x s) ,(sprite-y s))
	(,(+ (sprite-x s) (sprite-width s)) ,(sprite-y s))
	(,(sprite-x s) ,(+ (sprite-height s) (sprite-y s)))
	(,(+ (sprite-x s) (sprite-width s)) ,(+ (sprite-y s) (sprite-height s)))))

    (define (overlap s1 s2)
      (or (any (lambda (xy)
		 (inside? (first xy) (second xy) s1))
	       (sprite->points s2))
	  (any (lambda (xy)
		 (inside? (first xy) (second xy) s2))
	       (sprite->points s1))))

    (let ((cs (map (cut sprite-name <>)
		   (filter (lambda (s2)
			     (overlap s s2))
			   ss))))
      (and (not (null? cs)) cs)))

  (define (check-for-collisions)
    (let loop ((s (map cdr *sprites*))
	       (cols '()))
      (cond ((null? s) cols)
	    ((collides? (car s) (remove (cut equal? (car s) <>) (map cdr  *sprites*)))
	     => (lambda (o) (loop (cdr s) (cons (list (sprite-name (car s)) o) cols))))
	    (else (loop (cdr s) cols)))))

  (define (remove-sprite! s-name)
    (alist-delete! s-name *sprites* equal?))

  (define (update-sprite! s)
    (set! *sprites* (alist-update! (sprite-name s) s *sprites*)))

  (define add-sprite! update-sprite!)

  ;; Event stuff

  (define (translate-mouse-motion-event type event)
    (let ((x (sdl2:mouse-motion-event-x event))
    	  (y (sdl2:mouse-motion-event-y event)))
      (if (equal? type 'mouse-motion)
    	  (list 'mouse 'moved x y))))

  (define (translate-mouse-button-event type event)
    (let ((b (sdl2:mouse-button-event-button event)))
      (list 'mousebutton
	    (if (equal? type 'mouse-button-down)
		'pressed 'released)
	    b)))


  (define (translate-key-event type event)
    (let ((k (sdl2:keyboard-event-sym event)))
      (list 'key
	    (if (equal? type sdl2-internals:SDL_KEYUP)
		'released 'pressed)
	    (cond ((equal? k 'up)
		   'up)
		  ((equal? k 'down)
		   'down)
		  ((equal? k 'left)
		   'left)
		  ((equal? k 'right)
		   'right)
		  ((equal? k 'escape)
		   'escape)
		  ((integer->char k)
		   => identity)
		  (else 'unknown)))))


  (define (translate-events event escape)
    (if (not event)
	#f
	  (case (sdl2:event-type event)
	    ((quit)	     
	     (escape 'quit))
	    ((mouse-motion)
	     (translate-mouse-motion-event (sdl2:event-type event) event))
	    ((mouse-button-down)
	     (translate-mouse-button-event (sdl2:event-type event) event))
	    ((mouse-button-up)
	     (translate-mouse-button-event (sdl2:event-type event) event))
	    ((key-down)
	     (translate-key-event (sdl2:event-type event) event))
	    ((key-up)
	     (translate-key-event (sdl2:event-type event) event)))))
  
 (define (collect-events)
    (let pump ((events '()))
      (let ((event (sdl2:make-event)))
	(if (sdl2:poll-event! event)
	    (pump (cons event events))
	    (reverse events)))))

  (define (event-handler minimum-wait)
    (lambda ()
      (let ((last (current-milliseconds)))
	(call-with-current-continuation
	 (lambda (escape)
	   (let loop ()
	     (let* ((now (current-milliseconds))
		    (dt (min (/ 1 30) (/ (- now last)
					 1000))))
	       (call-with-current-continuation
		(lambda (k)
		  (with-exception-handler
		      (lambda (e)
			(fprintf (current-error-port) "Exception in world-changes: ~a, disabling world-changes.~%" ((condition-property-accessor 'exn 'message) e))
			(print-call-chain (current-error-port))
			(k (world-changes values)))
		    (lambda ()
		      (*world-changes*
		       (map (cut translate-events <> escape)
			    (collect-events))
		       dt
		       escape)))))
	       (show!)
	       (set! last now)
	       (when (< (- now last) minimum-wait)
		 (thread-sleep! (- minimum-wait (- now last))))
	       (loop)))))
	(call-with-current-continuation
	 (lambda (k)
	   (with-exception-handler
	       (lambda (e)
		 (fprintf (current-error-port) "Exception in world-ends: ~a, ignoring world-ends.~%" ((condition-property-accessor 'exn 'message) e))
		 (print-call-chain (current-error-port))
		 (k (world-ends values)))
	     (lambda ()
	       (*world-ends*)
	       ))))

	(sdl2:quit!))))

  (define (run-event-loop #!key
			  (run-in-background #f)
			  (minimum-wait *minimum-wait*))
    (set! *minimum-wait* minimum-wait)
    (call-with-current-continuation
     (lambda (k)
       (with-exception-handler
	   (lambda (e)
	     (fprintf (current-error-port) "Exception in world-inits: ~a, ignoring world-inits.~%" ((condition-property-accessor 'exn 'message) e))
	     (print-call-chain (current-error-port))
	     (k (world-inits values)))
	 (lambda ()
	   (*world-inits*)))))
    (sdl2:update-window-surface! *w*)
    (if run-in-background
	(thread-start!
	 (make-thread (event-handler minimum-wait) "doodle-event-loop"))
	(thread-join!
	 (thread-start!
	  (make-thread (event-handler minimum-wait) "doodle-event-loop")))))

  (define (doodle2-ticks)
    (sdl2:get-ticks))
  
  (define (draw-circle cx cy r num-segments col)

    (gl:Color4f (car col)
		(car (cdr col))
		(car (cddr col))
		(car (cdddr col)))
  
    (gl:Begin gl:LINE_LOOP)
    
    (let ((x 0)
	  (y 0)
	  (theta 0))
      (do((i 0 (add1 i)))
	  ((> i num-segments))	
	(set! theta (/ ( * 2 pi i) num-segments))
	(set!  x (* r (cos theta)))
	(set!  y (* r (sin theta)))
	(gl:Vertex2f (+ x cx) (+ y cy))))
    (gl:End))


  (define (draw-line x1 y1 x2 y2 col thickness)
    (gl:Begin gl:LINES)
    (gl:Color4f (car col)
                (car (cdr col))
                (car (cddr col))
                (car (cdddr col)))
    
    (gl:LineWidth thickness) 
    (gl:Vertex3f x1 y1 0.0)
    (gl:Vertex3f x2 y2 0)
    (gl:End))

  (define (rectangle x0 y0 width height col thickness #!key (filled #f))
    (if (eq? filled #t)
        (gl:PolygonMode gl:FRONT_AND_BACK gl:FILL)
        (gl:PolygonMode gl:FRONT_AND_BACK gl:LINE))
    (gl:Color4f (car col)
                (car (cdr col))
                (car (cddr col))
                (car (cdddr col)))
    (gl:Rectf x0 y0 width height)))
