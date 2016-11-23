;; Doodle2 - Drawing and gaming made easy
;;
;; Copyright (c) 2016, Anes Lihovac (changed to SDL2)
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
     blit-image
     check-for-collisions
     circle
     clear-screen
     current-background
     doodle-width
     doodle-height
     draw-line
     define-resource
     filled-circle
     filled-rectangle
     font-color
     font-size
     make-sprite
     line-width
     new-doodle
     rectangle
     remove-sprite!
     run-event-loop
     save-screenshot
     set-font!
     show!
     solid-black
     solid-white
     text
     text-width
     update-sprite!
     world-changes
     world-inits
     world-ends
     world-update-delay)

  (import chicken scheme)
  (use (srfi 1 4 18) data-structures extras clojurian-syntax matchable)
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

  (define *r* #f) ;sdl renderer
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

  (define (text x y text #!key (align #:left))
    (define (overall-height text)
      (let ((l (if (list? text) text (list text))))
	(fold (lambda (t s)
		(let-values (((w h) (text-width t)))
		  (+ s (* 1.5 h))))
	      0
	      l)))


  (define (save-screenshot filename)
    (pp "TODO"))
    

  (define (new-doodle2 #!key
		      (width 680)
		      (height 460)
		      (title "Doodle2")
		      (background solid-black)
		      (fullscreen #f))

    (sdl2:set-main-ready!)
    (sdl2:init! '(video))
    (let-values (( window renderer (sdl2:create-window-and-renderer! width height -1 'opengl)))
      (set! *w* window)
      (set! *r* renderer)
      (set! (sdl2:window-title *w*) title)))

    (set! doodle2-width width)
    (set! doodle2-height height)

    (current-background background)

    (gl:Flush)
    (sdl-gl-swap-buffers))

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
        (gl:Clear (+ gl:COLOR_BUFFER_BIT gl:DEPTH_BUFFER_BIT)))
	   
  (define (show!)

    (sdl2:show-window! *w*)
    (gl:Flush)
    (sdl-gl-swap-buffers))


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

  ;; blitting tiles in

  (define *resources* '())

  (define-record img-res name file surface w h x-offset y-offset scale-factor)
  (define-record-printer (img-res i out)
    (fprintf out "#,(img-res ~a ~s ~ax~a px (~ax~a) x ~a"
	     (img-res-name i)
	     (img-res-file i)
	     (img-res-w i)
	     (img-res-h i)
	     (img-res-x-offset i)
	     (img-res-y-offset i)
	     (img-res-scale-factor i)))


  (define (define-resource name type file . data)
    (when (not (file-exists? file))
      (error "Resource does not exist " file))
    (match type
      ('#:image
       (let* ((s (cairo-image-surface-create-from-png file))
	      (w (cairo-image-surface-get-width s))
	      (h (cairo-image-surface-get-height s))
	      (offset? (and (not (null? data))
			    (>= (length data) 2)))
	      (x-off (if offset? (car data) 0))
	      (y-off (if offset? (cadr data) 0))
	      (scale-factor (and (= (length data) 3)
				 (caddr data))))
	 (set! *resources*
	   (alist-update! name (make-img-res name file s w h x-off y-off scale-factor)
			  *resources*))))
      ('#:tileset
       (when (or (null? data)
		 (not (= (length data) 2)))
	 (error "Tileset needs tile-size and list of tiles but got " data))
       (let* ((ts (cairo-image-surface-create-from-png file))
	      (w (cairo-image-surface-get-width ts))
	      (h (cairo-image-surface-get-height ts))
	      (tile-size
	       (if (number? (car data))
		   (car data)
		   (error "Tile-set parameter needs to be a number")))
	      (tiles (if (list? (cadr data))
			 (cadr data)
			 (error "Second data parameter must be a list of tiles"))))
	 (for-each
	  (lambda (t)
	    (if (and (list t)
		     (= (length t) 2))
		(let* ((tile-name (car t))
		       (tile-number (cadr t))
		       (tile-x (modulo (* tile-number tile-size) w))
		       (tile-y (* tile-size (quotient (* tile-size tile-number) w)))
		       (tile-img
			(cairo-surface-create-for-rectangle ts tile-x tile-y tile-size tile-size)))
		  (set! *resources*
		    (alist-update! (car t) (make-img-res tile-name file tile-img tile-size tile-size 0 0 #f)
				   *resources*)))
		(error "Malformed tile information entry " t)))
	  tiles)))
      (else (error "Unkown resource type " type))))

  (define (blit-image name x y #!key (rotation 0))
    (let ((img (alist-ref name *resources* equal?)))
      (unless img
	(error "No resource found with this name " name))
      (let* ((scale (img-res-scale-factor img))
	     (scale (if scale scale 1))
	     (h (img-res-h img))
	     (w (img-res-w img))
	     (t (/ (- 1 scale) 2))
	     (x (+ x (* t w)
		   (img-res-x-offset img)))
	     (y (+ y (* t h)
		   (img-res-y-offset img))))
	(cairo-save *c*)
	(if (zero? rotation)
	    (cairo-translate *c* x y)
	    (doto *c*
		  (cairo-translate (+ (/ w 2) x)
				   (+ (/ h 2) y))
		  (cairo-rotate (* (/ cairo-pi 180) rotation))
		  (cairo-translate (- (/ w 2)) (- (/ h 2)))))
	(doto *c*
	      (cairo-scale scale scale)
	      (cairo-set-source-surface (img-res-surface img) 0 0)
	      (cairo-mask-surface (img-res-surface img) 0 0)
	      (cairo-restore)))))

  ;; Event stuff

  (define (translate-mouse-event type event)
    (let ((x (sdl-event-x event))
	  (y (sdl-event-y event)))
      (if (equal? type SDL_MOUSEMOTION)
	  (list 'mouse 'moved x y)
	  (let ((b (sdl-event-button event)))
	    (list 'mouse
		  (if (equal? type SDL_MOUSEBUTTONDOWN)
		      'pressed 'released)
		  x y b)))))

  (define (translate-key-event type event)
    (let ((k (sdl-event-sym event)))
      (list 'key
	    (if (equal? type SDL_KEYUP)
		'released 'pressed)
	    (cond ((equal? k SDLK_UP)
		   'up)
		  ((equal? k SDLK_DOWN)
		   'down)
		  ((equal? k SDLK_LEFT)
		   'left)
		  ((equal? k SDLK_RIGHT)
		   'right)
		  ((integer->char k)
		   => identity)
		  (else 'unknown)))))

  (define (translate-events event escape)
    (if (not event)
	#f
	(let ((t (sdl-event-type event)))
	  (cond ((equal? t SDL_QUIT)
		 (escape 'quit))
		((equal? t SDL_VIDEOEXPOSE)
		 '(redraw))
		((or (equal? t SDL_MOUSEBUTTONUP)
		     (equal? t SDL_MOUSEBUTTONDOWN)
		     (equal? t SDL_MOUSEMOTION))
		 (translate-mouse-event t event))
		((or (equal? t SDL_KEYDOWN)
		     (equal? t SDL_KEYUP))
		 (translate-key-event t event))
		(else (list 'unknown event))))))

  (define (collect-events)
    (let pump ((events '()))
      (let ((event (make-sdl-event)))
	(if (sdl-poll-event! event)
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
	(sdl-quit))))

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
    (sdl-flip *s*)
    (gl:Flush)
    (sdl-gl-swap-buffers)
    (if run-in-background
	(thread-start!
	 (make-thread (event-handler minimum-wait) "doodle-event-loop"))
	(thread-join!
	 (thread-start!
	  (make-thread (event-handler minimum-wait) "doodle-event-loop"))))))
