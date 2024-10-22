#!/usr/bin/env janet
(use jaylib)

(def screen-width 600)
(def screen-height 600)
(def max-bullet-age 60)
(var state nil)
(var frame-counter -1)

(math/seedrandom (os/cryptorand 8))

(defn engine/loop [init-fn update-fn draw-fn width height window-title]
  (init-window width height window-title)
  (set-target-fps 60)
  (init-fn)
  (while (not (window-should-close))
    (update-fn)
    (draw-fn))
  (close-window))

(defn asteroid-radius [asteroid]
  (* (asteroid :size) 10))

##############
# math utils #
##############

(defn vector-add [v1 v2]
  [(+ (v1 0) (v2 0))
   (+ (v1 1) (v2 1))])

(defn vector-sub [v1 v2]
  [(- (v1 0) (v2 0))
   (- (v1 1) (v2 1))])

(defn vector-mul [v1 v2]
  [(* (v1 0) (v2 0))
   (* (v1 1) (v2 1))])

(defn vector-wrap [v modulus]
  [(if (< (v 0) 0)
     (+ 600 (v 0))
     (% (math/abs (v 0)) modulus))
   (if (< (v 1) 0)
     (+ 600 (v 1))
     (% (math/abs (v 1)) modulus))])

(defn point-distance [a b]
  (math/sqrt
    (+
      (math/pow (- (a 0) (b 0)) 2)
      (math/pow (- (a 1) (b 1)) 2))))

(defn point-in-circle? [point circle]
  (let [distance (point-distance point (circle :center))
        radius (circle :radius)]
    (< distance radius)))

################
# constructors #
################

(defn asteroid-speed [size]
  (let [base-speed 1]
    (var speed base-speed)
    (repeat (- 3 size) (set speed (* 1.5 speed)))
    speed))

# velocity based on random x and y components
(defn random-asteroid-velocity0 [size]
  (let [speed (asteroid-speed size)]
    [(* speed (- (math/random) 0.5))
     (* speed (- (math/random) 0.5))]))

# veolcity based on fixed speed in a random direction
(defn random-asteroid-velocity [size]
  (let [speed (asteroid-speed size)
        angle (* (math/random) math/pi)]
    [(* speed (math/cos angle))
     (* speed (math/sin angle))]))

(defn make-asteroid [&opt x y size velocity]
  (default size 3)
  (default x (* (* screen-width 0.7) (- (math/random) 0.5)))
  (default y (* (* screen-height 0.7) (- (math/random) 0.5)))
  (default velocity (random-asteroid-velocity size))
  @{:size size
    :position [x y]
    :velocity velocity})

(defn make-bullet [pos velocity]
  @{:position pos
    :velocity velocity
    :age 0})

(defn make-ship [width height]
  @{:size 30
    :aspect 0.8
    :position [(/ width 2) (/ height 2)]
    :orientation 0.0
    :velocity [0.0 0.0]})

(defn spawn-asteroid [&opt size position]
  (let [x (position 0)
        y (position 1)]
    (array/push (state :asteroids) (make-asteroid x y size))))

(defn make-state [width height]
  (let [canvas (gen-image-color width height :white)]
    @{:screen-width width
      :screen-height height
      :canvas canvas
      :texture (load-texture-from-image canvas)
      :ship (make-ship width height)
      :asteroids (map (fn [_] (make-asteroid)) (range 3))
      :bullets @[]
      :alive true}))

(defn set-state [key value]
  (set (state key) value))

(defn get-state [key]
  (state key))

(defn ship-thrust-vector []
  (let [angle ((state :ship) :orientation)
        magnitude 0.05]
    [(* (math/cos angle) magnitude)
     (* (math/sin angle) magnitude)]))

(defn bullet-firing-vector []
  (let [angle ((state :ship) :orientation)
        magnitude 5]
    [(* (math/cos angle) magnitude)
     (* (math/sin angle) magnitude)]))

(defn move-ship []
  (let [ship (state :ship)]
    (set (ship :position)
         (vector-wrap (vector-add (ship :position) (ship :velocity)) screen-width))))

(defn move-bullet [i]
  (let [bullet ((state :bullets) i)
        new-position (vector-add (bullet :position) (bullet :velocity))]
      (set (bullet :position) (vector-wrap new-position screen-width))
      (set (bullet :age) (inc (bullet :age)))))

(defn move-bullets []
  (var i 0)
  (def len (length (state :bullets)))
  (while (< i len)
    (move-bullet i)
    (++ i)))

(defn move-asteroid [asteroid]
  (set (asteroid :position)
       (vector-wrap (vector-add (asteroid :position) (asteroid :velocity)) (state :screen-width))))

(defn move-asteroids []
  (loop [asteroid :in (state :asteroids)]
    (move-asteroid asteroid)))

(defn find-ship-center []
  (let [ship (state :ship)
        ship-position (ship :position)
        ship-x (ship-position 0)
        ship-y (ship-position 1)
        ship-size (ship :size)
        ship-aspect (ship :aspect)]
    [(+ ship-x (/ ship-size 3))
     (+ ship-y (* (/ ship-size 2) ship-aspect))]))

(defn rotate-ship-point [point]
  (let [ship (state :ship)
        x1 (point 0)
        y1 (point 1)
        center (find-ship-center)
        x0 (center 0)
        y0 (center 1)
        theta (ship :orientation)]
    [(- (* (- x1 x0) (math/cos theta)) (* (- y1 y0) (math/sin theta)))
     (+ (* (- y1 y0) (math/cos theta)) (* (- x1 x0) (math/sin theta)))]))

(defn ship-points []
  (let [ship (state :ship)
        ship-position (ship :position)
        ship-x (ship-position 0)
        ship-y (ship-position 1)
        ship-size (ship :size)
        ship-aspect (ship :aspect)
        ship-center (find-ship-center)
        ship-length ship-size
        ship-width (* ship-size ship-aspect)
        p1 [ship-x ship-y]
        p2 [ship-x (+ ship-y ship-width)]
        p3 [(+ ship-x ship-length) (+ ship-y (/ ship-width 2))]
        rotated-points (map (fn [p] (rotate-ship-point p)) [p1 p2 p3])
        translated-rotated-points (map (fn [p] (vector-add ship-center p)) rotated-points)]
    translated-rotated-points))

(defn thrust-points []
  (let [ship (state :ship)
        ship-position (ship :position)
        ship-x (ship-position 0)
        ship-y (ship-position 1)
        ship-size (ship :size)
        ship-aspect (ship :aspect)
        ship-center (find-ship-center)
        ship-center-x (ship-center 0)
        ship-center-y (ship-center 1)
        ship-length ship-size
        ship-width (* ship-size ship-aspect)
        p1 [(- ship-center-x 12) (+ ship-center-y 3)]
        p2 [(- ship-center-x 24) ship-center-y]
        p3 [(- ship-center-x 12) (- ship-center-y 3)]
        rotated-points (map (fn [p] (rotate-ship-point p)) [p1 p3 p2])
        translated-rotated-points (map (fn [p] (vector-add ship-center p)) rotated-points)]
      translated-rotated-points))

(defn retro-thrust-points1 []
  (let [ship (state :ship)
        ship-center (find-ship-center)
        ship-center-x (ship-center 0)
        ship-center-y (ship-center 1)
        p1 [ship-center-x (- ship-center-y 7)]
        p2 [(+ ship-center-x 10) (- ship-center-y 9)]
        p3 [ship-center-x (- ship-center-y 11)]
        rotated-points (map (fn [p] (rotate-ship-point p)) [p1 p2 p3])
        translated-rotated-points (map (fn [p] (vector-add ship-center p)) rotated-points)]
    translated-rotated-points))

(defn retro-thrust-points2 []
  (let [ship (state :ship)
        ship-center (find-ship-center)
        ship-center-x (ship-center 0)
        ship-center-y (ship-center 1)
        p1 [ship-center-x (+ ship-center-y 7)]
        p2 [(+ ship-center-x 10) (+ ship-center-y 9)]
        p3 [ship-center-x (+ ship-center-y 11)]
        rotated-points (map (fn [p] (rotate-ship-point p)) [p1 p3 p2])
        translated-rotated-points (map (fn [p] (vector-add ship-center p)) rotated-points)]
    translated-rotated-points))

##############
# collisions #
##############

(defn asteroid-collides-ship? [asteroid]
  (let [circle @{:center (asteroid :position) :radius (asteroid-radius asteroid)}]
    (any? (map (fn [point] (point-in-circle? point circle)) (ship-points)))))

(defn ship-collides? []
  (any? (map asteroid-collides-ship? (state :asteroids))))

(defn handle-ship-collisions []
  (if (ship-collides?)
    (set (state :alive) false)))

(defn asteroid-collides-bullet? [asteroid bullet]
  (let [circle @{:center (asteroid :position) :radius (asteroid-radius asteroid)}]
    (point-in-circle? (bullet :position) circle)))

(defn find-bullet-collisions []
  (let [collisions @[]]
    (loop [bullet :in (state :bullets)]
      (loop [asteroid :in (state :asteroids)]
        (if (asteroid-collides-bullet? asteroid bullet)
          (array/push collisions [asteroid bullet]))))
    collisions))

(defn explode-asteroid [asteroid]
  (let [new-asteroids @[]]
    (if (> (asteroid :size) 1)
      (do
        (spawn-asteroid (- (asteroid :size) 1) (asteroid :position))
        (spawn-asteroid (- (asteroid :size) 1) (asteroid :position))))
    (set (state :asteroids) (filter (fn [item] (not (= asteroid item))) (state :asteroids)))))

(defn handle-bullet-collisions []
  (let [collisions (find-bullet-collisions)
        colliding-bullets (map last collisions)
        colliding-asteroids (map first collisions)
        remaining-bullets (filter (fn [bullet] (not (index-of bullet colliding-bullets))) (state :bullets))]
    (set (state :bullets) remaining-bullets)
    (each asteroid colliding-asteroids (explode-asteroid asteroid))))

(defn spawn-bullet [ship]
  (let [bullet-velocity (vector-add ((state :ship) :velocity) (bullet-firing-vector))]
    (make-bullet (find-ship-center) bullet-velocity)))

##############
# draw calls #
##############

(defn draw-asteroid [asteroid]
  (draw-circle
    (splice (map math/floor (asteroid :position)))
    (asteroid-radius asteroid)
    :white))

(defn draw-asteroids []
  (loop [asteroid :in (state :asteroids)]
    (draw-asteroid asteroid)))

(defn draw-thrust []
  (draw-triangle (splice (thrust-points)) :orange))

(defn draw-retro-thrust []
  (draw-triangle (splice (retro-thrust-points1)) :orange)
  (draw-triangle (splice (retro-thrust-points2)) :orange))

(defn draw-ship []
  (let [is-alternate-frame (< (% frame-counter 5) 3)]
    (if (and (key-down? :up) is-alternate-frame)
      (draw-thrust))
    (if (and (key-down? :down) is-alternate-frame)
      (draw-retro-thrust))
    (draw-triangle (splice (ship-points)) :white)))

(defn draw-bullet [bullet]
  (draw-circle (splice (map math/floor (bullet :position))) 2 :white))

(defn draw-bullets []
  (loop [bullet :in (state :bullets)]
    (draw-bullet bullet)))

(defn draw-wasted []
  (let [text "wasted"
        font-size 48
        text-width (measure-text text font-size)
        x (- (/ screen-width 2) (/ text-width 2))
        y (/ screen-height 2)]
    (draw-text text (math/floor x) (math/floor y) font-size :red)))

(defn cull-bullets []
  (set (state :bullets)
       (filter (fn (x) (< (x :age) max-bullet-age)) (state :bullets))))

(defn my-init []
  (set state (make-state screen-width screen-height)))
    
(defn handle-keyboard-input []
  (let [ship (state :ship)]
    (if (key-down? :left) (set (ship :orientation) (- (ship :orientation) 0.05)))
    (if (key-down? :right) (set (ship :orientation) (+ (ship :orientation) 0.05)))
    (if (key-down? :up)
      (set (ship :velocity) (vector-add (ship :velocity) (ship-thrust-vector))))
    (if (key-down? :down)
      (set (ship :velocity) (vector-sub (ship :velocity) (ship-thrust-vector))))
    (if (key-pressed? :space)
      (if (state :alive)
        (array/push (state :bullets) (spawn-bullet ship))
        (my-init)))))

######################
# grip it and rip it #
######################

(defn update-state []
  (++ frame-counter)
  (handle-keyboard-input)
  (move-ship)
  (move-bullets)
  (move-asteroids)
  (handle-ship-collisions)
  (cull-bullets)
  (handle-bullet-collisions))

(defn draw []
  (begin-drawing)
  (clear-background :black)
  (if (state :alive)
    (draw-ship)
    (draw-wasted))
  (draw-bullets)
  (draw-asteroids)
  (end-drawing))

(engine/loop my-init update-state draw screen-width screen-height "Asteroids")
