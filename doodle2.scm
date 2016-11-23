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
;     save-screenshot
     show!
     solid-black
     solid-white
;     text
     update-sprite!
     world-changes
     world-inits
     world-ends
     world-update-delay)

  (import chicken scheme)
  (use (srfi 1 4 18) data-structures extras clojurian-syntax matchable)

  (use (prefix sdl2-internals sdl2-internals:))
  (use (prefix sdl2 sdl2:))

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
  (define solid-white (list 1 1 1 1))

;  (define *r* #f) ;sdl renderer
  (define *w* #f) ;sdl window

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

  ;; (define (text x y text #!key (align #:left))
  ;;   (define (overall-height text)
  ;;     (let ((l (if (list? text) text (list text))))
  ;; 	(fold (lambda (t s)
  ;; 		(let-values (((w h) (text-width t)))
  ;; 		  (+ s (* 1.5 h))))
  ;; 	      0
  ;; 	      l))))


  (define (save-screenshot filename)
    (pp "TODO"))
    

  (define (new-doodle2 #!key
		      (width 680)
		      (height 460)
		      (title "Doodle2")
;		      (background solid-black)
		      (fullscreen #f))

    (sdl2:set-main-ready!)
    (sdl2:init! '(everything))
;    (let-values ((window renderer (sdl2:create-window-and-renderer! width height -1 'opengl)))
    (let-values ((window (sdl2:create-window! title -1 'opengl width height)))
      (set! *w* window)
;      (set! *r* renderer)
      (set! (sdl2:window-title *w*) title))

    (set! doodle2-width width)
    (set! doodle2-height height)

;    (current-background background)

    (sdl2:update-window-surface! *w*))
;    (gl:Flush)
;    (sdl-gl-swap-buffers)
    

  ;; (define (clear-screen #!optional (color *current-background*))
  ;;   (let ((width (cairo-image-surface-get-width *c-surface*))
  ;; 	  (height (cairo-image-surface-get-height *c-surface*)))
  ;;     (if (list? color)
  ;; 	  (begin
  ;; 	    (apply cairo-set-source-rgba (cons *c* color))
  ;; 	    (doto *c*
  ;; 		  (cairo-rectangle 0 0 width height)
  ;; 		  (cairo-fill)
  ;; 		  (cairo-stroke)))
  ;; 	  (blit-image (string->symbol color) 0 0))))

  (define (clear-screen)
    (pp "clear-screen: todo")
    ;(gl:Clear (+ gl:COLOR_BUFFER_BIT gl:DEPTH_BUFFER_BIT))
    )
	   
  (define (show!)

    (sdl2:show-window! *w*)
;    (gl:Flush)
;    (sdl-gl-swap-buffers)
    )


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

  (define (translate-mouse-event type event)
    (let ((x (sdl2:mouse-motion-event-x event))
	  (y (sdl2:mouse-motion-event-y event)))
      (if (equal? type sdl2-internals:SDL_MOUSEMOTION)
	  (list 'mouse 'moved x y)
	  (let ((b (sdl2:mouse-button-event-button event)))
	    (list 'mouse
		  (if (equal? type sdl2-internals:SDL_MOUSEBUTTONDOWN)
		      'pressed 'released)
		  x y b)))))


    (define (translate-key-event type event)
    (let ((k (sdl2:keyboard-event-sym event)))
      (list 'key
	    (if (equal? type sdl2-internals:SDL_KEYUP)
		'released 'pressed)
	    (cond ((equal? k sdl2-internals:SDLK_UP)
		   'up)
		  ((equal? k sdl2-internals:SDLK_DOWN)
		   'down)
		  ((equal? k sdl2-internals:SDLK_LEFT)
		   'left)
		  ((equal? k sdl2-internals:SDLK_RIGHT)
		   'right)
		  ((integer->char k)
		   => identity)
		  (else 'unknown)))))
    
 (define (translate-events event escape)
    (if (not event)
	#f
	(let ((t (sdl2:event-type event)))
	  (cond ((equal? t sdl2-internals:SDL_QUIT)
		 (escape 'quit))
		((equal? t sdl2-internals:SDL_WINDOWEVENT_EXPOSED)
		 '(redraw))
		((or (equal? t sdl2-internals:SDL_MOUSEBUTTONUP)
		     (equal? t sdl2-internals:SDL_MOUSEBUTTONDOWN)
		     (equal? t sdl2-internals:SDL_MOUSEMOTION))
		 (translate-mouse-event t event))
		((or (equal? t sdl2-internals:SDL_KEYDOWN)
		     (equal? t sdl2-internals:SDL_KEYUP))
		 (translate-key-event t event))
		(else (list 'unknown event))))))

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
	       (*world-ends*)))))
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
;    (gl:Flush)
;    (sdl-gl-swap-buffers)
    (if run-in-background
	(thread-start!
	 (make-thread (event-handler minimum-wait) "doodle-event-loop"))
	(thread-join!
	 (thread-start!
	  (make-thread (event-handler minimum-wait) "doodle-event-loop"))))))

