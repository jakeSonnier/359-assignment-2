#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

;;Jake Sonnier C00299868

;; Graphical and Physical constants
(define GAME-HEIGHT 200) ;;Defines the height of the area for editing
(define GAME-WIDTH 200) ;;Same but for width
(define TANK-HEIGHT (- GAME-HEIGHT 15))
(define TANK-START-X (/ GAME-WIDTH 2))
(define UFO-START-HEIGHT 30)
(define UFO-START-X (/ GAME-WIDTH 2))
(define UFO-IMAGE (rectangle 30 30 "solid" "black"))
(define TANK-IMAGE (rectangle 20 20 "solid" "green"))
(define MISSILE-IMAGE (rectangle 5 10 "solid" "green"))
;; Missiles move 30 units upwards per tick
(define MISSILE-SPEED 30)
;;UFO descends 2 units per tick
(define UFO-SPEED 2)
;; Some default velocities for moving the tank left and right
(define TANK-NEG-VEL -8)
(define TANK-POS-VEL 8)
(define BACKGROUND (empty-scene GAME-WIDTH GAME-HEIGHT "white"))

;; Be sure to add data examples for the data definitions below!

;; point is a structure
;; (point Number Number)
;; Interpretation, it represents a cartesian point
;; of positive values. In rendering higher Y
;; means lower height in the scene.
(struct point [x y] #:transparent)

;; Don't worry about point examples, just do UFO and Missile examples

;; A UFO is a Point
;; interpretation (point x y) is the UFO's location
;; (using the top-down, left-to-right convention)

;; Add data examples for UFOs here

;; A Missile is a Point.
;; interpretation (point x y) is the missile's place

;; Add data examples for Missiles here



;; A Tank is a structure:
;; (tank Number Number).
;; interpretation (tank x dx) specifies the position:
;; (x, HEIGHT) and the tank's speed: dx pixels/tick
(struct tank [loc vel] #:transparent)

;; Add data examples for Tanks here

;; aim is a structure
;; (aim UFO Tank)
;; Interpretation, UFO represents the UFO position
;; Tank represents the Tank position.
(struct aim [ufo tank] #:transparent)

;; Add data examples for Aim states here

;; fired is a structure
;; (fired UFO Tank)
;; Interpretation, UFO represents the UFO position
;; Tank represents the Tank position.
(struct fired [ufo tank missile] #:transparent)

;; Add data examples for Fired states here

;; A SIGS is one of: 
;; – (aim UFO Tank)
;; – (fired UFO Tank Missile)
;; interpretation represents the complete state of a 
;; space invader game

;; Add more relevant examples of aim and fired states here

;; Tank -> Image
;; Draw a tank on the background
(define (draw-tank tank-data)
  (place-image/align
        TANK-IMAGE
        (tank-loc tank-data)
        TANK-HEIGHT
        "center"
        "center" BACKGROUND))

;; UFO Image -> Image
;; After the tank is drawn,
;; draw the ufo into the given image
(define (draw-ufo ufo-data tank-drawn)
  (place-image/align
      UFO-IMAGE
      (point-x ufo-data)
      (point-y ufo-data)
      "center"
      "center"
      tank-drawn))

;; Missile Image -> Image
;; After the UFO and tank are drawn
;; pass the missile data for drawing
;; onto the image with the UFO and tank
(define (draw-missile missile-data ufo-drawn)
  (place-image/align
      MISSILE-IMAGE
      (point-x missile-data)
      (point-y missile-data)
      "center"
      "center"
      ufo-drawn))


;;SIGS -> Image
;;Renders the game by drawing either a tank and a ufo
;;or a tank, ufo, and missile
(define (render game-state)
  (cond
    [(aim? game-state)
     (define tank-data (aim-tank game-state))
     (define ufo-data (aim-ufo game-state))
     (define tank-drawn (draw-tank tank-data))
     (draw-ufo ufo-data tank-drawn)]
    [(fired? game-state)
     (define tank-data (fired-tank game-state))
     (define ufo-data (fired-ufo game-state))
     (define missile-data (fired-missile game-state))
     (define tank-drawn (draw-tank tank-data))
     (define ufo-drawn (draw-ufo ufo-data tank-drawn))
     (draw-missile missile-data ufo-drawn)]))

;; Void -> Int
;; Produces a random number in [8, 8]
(define (random-jump) (random -8 9))


;; Below is an example of how to generate a random number from [-8, 8]
(define random-number-example (random-jump))

;; GameState KeyEvent -> GameState
;; update the tanks velocity or shoot a missile
(define (key-handler game-state ke)
  (cond
    [(equal? ke "left") (tank tank-vel TANK-NEG-VEL)]
    [(equal? ke "right") (tank tank-vel TANK-POS-VEL)]
  )
  (cond
    [(aim? game-state)
     (cond
       [(equal? ke " ") game-state]
       )
     ]
  )
 )


;; SIGS -> SIGS
;; Move the ufo and missiles as time goes on
(define (update game-state)  
  (cond
    [(aim? game-state)
     ;;New ufo x,y
     (define new-ufo-x (+(point-x (aim-ufo game-state))(random-jump)))
     (define new-ufo-y (+(point-y (aim-ufo game-state))1))
     ;;New tank loc
     (define new-tank-loc (+(tank-loc (aim-tank game-state))TANK-NEG-VEL))
     ;;New ufo and tank
     (define new-ufo (point new-ufo-x new-ufo-y))
     (define new-tank (tank new-tank-loc TANK-NEG-VEL))
     ;;New aim game-state
     (define new-aim (aim new-ufo new-tank))new-aim]
    [(fired? game-state)
     ;;New missile x,y
     (define new-missile-x (point-x (fired-missile game-state)))
     (define new-missile-y (+(point-y (fired-missile game-state))MISSILE-SPEED))
     ;;New ufo x,y
     (define new-ufo-x (+(point-x (aim-ufo game-state))(random-jump)))
     (define new-ufo-y (+(point-y (aim-ufo game-state))1))
     ;;New tank loc
     (define new-tank-loc (+(tank-loc (aim-tank game-state))TANK-NEG-VEL))
     ;;New ufo and tank and missile
     (define new-ufo (point new-ufo-x new-ufo-y))
     (define new-tank (tank new-tank-loc TANK-NEG-VEL))
     (define new-missile (point new-missile-x new-missile-y))
     ;;New aim game-state
     (define new-fired (fired new-ufo new-tank))
     (define new-aim (aim new-ufo new-tank))(if (<= new-missile-y 0) new-aim new-fired)]))


;; UFO Missile -> Bool
;; Checks if the missile collides with the ufo
(define (hit-ufo? ufo-data missile-data)
  (define ufo-x (point-x ufo-data))
  (define missile-x (point-x missile-data))
  (and
   (<= missile-x (+ ufo-x 15)) ;;a ufo has width 30 and rendering happens at center
   (>= missile-x (- ufo-x 15)) 
   (<= (point-y missile-data) (- (point-y ufo-data) 15))))
(check-expect (hit-ufo? (point 96 66) (point 100 20)) #t)
(check-expect (hit-ufo? (point 96 66) (point 100 GAME-HEIGHT)) #f)

;; UFO -> Bool
;; Checks if the ufo gets past the tank
(define (ufo-bottom? ufo-data)
  (>= (+ (point-y ufo-data) 15)  (- TANK-HEIGHT 10))) ;; the tank image has height 20 and is centered
(check-expect (ufo-bottom? (point 133 160)) #t)
(check-expect (ufo-bottom? (point 133 0)) #f)


;; SIGS -> Bool
;; Checks if the game ends because a missile hit
;; the ufo or the ufo gets past the tank
(define (end-game? game-state)
  (match game-state
    [(fired ufo-data tank-data missile-data)
     (or
      (hit-ufo? ufo-data missile-data)
      (ufo-bottom? ufo-data))]
    [(aim ufo-data tank-data)
     (ufo-bottom? ufo-data)]))

(check-expect (end-game? (fired (point 96 66) (tank 100 0) (point 100 20))) #t)
(check-expect (end-game? (fired (point 96 66) (tank 100 0) (point 100 GAME-HEIGHT))) #f)
(check-expect (end-game? (aim (point 133 160) (tank 52 0))) #t)
(check-expect (end-game? (aim (point 133 0) (tank 52 0))) #f)

;; How you can run the game
;; Feel free to update the clock tick speed how you like
(define (run game-state)
  (big-bang game-state
            [on-tick update 0.1]
            [to-draw render]
            [on-key key-handler]
            [stop-when end-game?]))

;; With these specified you can start the game with run
(define UFO-EX1 (point UFO-START-X UFO-START-HEIGHT))
(define TANK-EX1 (tank TANK-START-X 0))
(define MISSILE-EX1 (point TANK-START-X (- TANK-HEIGHT 15)))
(define AIM-EX1 (aim UFO-EX1 TANK-EX1))
(define FIRED-EX1 (fired UFO-EX1 TANK-EX1 MISSILE-EX1))

;;You can test render with the following calls
(render AIM-EX1)
(render FIRED-EX1)

;; Runs the program from a start state
;; Uncomment to see your program running
 (run AIM-EX1)


