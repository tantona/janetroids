#!/usr/bin/env janet
(use jaylib)

(def screen-width 600)
(def screen-height 600)
(def max-bullet-age 60)
(var state nil)

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
  [(+ (get v1 0) (get v2 0)) (+ (get v1 1) (get v2 1))])

(defn vector-mul [v1 v2]
  [(* (get v1 0) (get v2 0)) (* (get v1 1) (get v2 1))])

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

(defn random-asteroid-velocity [size]
  (let [base-speed 3]
    (var speed base-speed)
    (repeat (- 3 size) (set speed (* 1.5 speed)))
    [(* speed (- (math/random) 0.5))
    (* speed (- (math/random) 0.5))]))

(defn make-asteroid [&opt x y size velocity]
  (default size 3)
  (default x (* screen-width (math/random)))
  (default y (* screen-height (math/random)))
  (default velocity (random-asteroid-velocity size))
  @{:size size
    :position [x y]
    :velocity velocity})

(defn make-bullet [pos velocity]
  @{:position pos
    :velocity velocity
    :age 0})

(defn make-ship [width height]
  @{:size 25
    :position [(/ width 2) (/ height 2)]
    :orientation 0.0
    :velocity [0.0 0.0]})

(defn spawn-asteroid [&opt size position]
  (let [x (position 0)
        y (position 1)]
    (array/push (state :asteroids) (make-asteroid x y size))))

(defn make-state [width height]
  (let [canvas (gen-image-color width height :ray-white)]
    @{:screen-width width
      :screen-height height
      :canvas canvas
      :texture (load-texture-from-image canvas)
      :ship (make-ship width height)
      :asteroids (map (fn [_] (make-asteroid)) (range 3))
      :bullets @[]}))

(defn set-state [key value]
  (set (state key) value))

(defn get-state [key]
  (state key))

(defn calculate-thrust-vector []
  [(* (math/cos ((state :ship) :orientation)) 0.05)
   (* (math/sin ((state :ship) :orientation)) 0.05)])

(defn calculate-ship-velocity []
  (vector-add ((state :ship) :velocity) (calculate-thrust-vector)))

(defn move-ship []
  (let [ship (state :ship)]
    (set (ship :position)
         (vector-wrap (vector-add (ship :position) (ship :velocity)) screen-width))))

(defn move-bullet [i]
  (let [bullet ((state :bullets) i)]
      (set (bullet :position)
           (vector-add (bullet :position)
                       (bullet :velocity)))
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
        ship-size (ship :size)]
    [(/ (+ ship-x ship-x (+ ship-x ship-size)) 3)
     (/ (+ ship-y (+ ship-y ship-size) (/ (+ (+ ship-y ship-size) ship-y) 2)) 3)]))

(defn rotate-point [point]
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
        ship-center (find-ship-center)
        p1 (vector-add (rotate-point [ship-x ship-y]) ship-center)
        p2 (vector-add (rotate-point [ship-x (+ ship-y ship-size)]) ship-center)
        p3 (vector-add (rotate-point [(+ ship-x ship-size) (/ (+ (+ ship-y ship-size) ship-y) 2)]) ship-center)]
    [p1 p2 p3]))

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
    (os/exit 1)))

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
        (spawn-asteroid (-- (asteroid :size)) (asteroid :position))
        (spawn-asteroid (-- (asteroid :size)) (asteroid :position))))
    (set (state :asteroids) (filter (fn [item] (not (= asteroid item))) (state :asteroids)))))

(defn handle-bullet-collisions []
  (let [collisions (find-bullet-collisions)
        colliding-bullets (map last collisions)
        colliding-asteroids (map first collisions)
        remaining-bullets (filter (fn [bullet] (index-of bullet colliding-bullets)) (state :bullets))]
        (set (state :bullets) remaining-bullets)
    (each asteroid colliding-asteroids (explode-asteroid asteroid))))

(defn spawn-bullet [ship]
  (let [v (vector-mul [100 100] (calculate-thrust-vector))]
    (make-bullet (find-ship-center) v)))

##############
# draw calls #
##############

(defn draw-asteroid [asteroid]
  (draw-circle
    (splice (map math/floor (asteroid :position)))
    (asteroid-radius asteroid)
    :ray-white))

(defn draw-asteroids []
  (loop [asteroid :in (state :asteroids)]
    (draw-asteroid asteroid)))

(defn draw-ship [state]
  (draw-triangle (splice (ship-points)) :ray-white))

(defn draw-bullet [bullet]
  (draw-circle (splice (map math/floor (bullet :position))) 2 :ray-white))

(defn draw-bullets []
  (loop [bullet :in (state :bullets)]
    (draw-bullet bullet)))

(defn cull-bullets []
  (set (state :bullets)
       (filter (fn (x) (< (x :age) max-bullet-age)) (state :bullets))))

(defn handle-keyboard-input []
  (let [ship (state :ship)]
    (if (key-down? :left) (set (ship :orientation) (- (ship :orientation) 0.05)))
    (if (key-down? :right) (set (ship :orientation) (+ (ship :orientation) 0.05)))
    (if (key-down? :up) (set (ship :velocity) (calculate-ship-velocity)))
    (if (key-pressed? :space) (array/push (state :bullets) (spawn-bullet ship)))))

######################
# grip it and rip it #
######################

(defn my-init []
  (set state (make-state screen-width screen-height)))

(defn update-state []
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
  (draw-ship state)
  (draw-bullets)
  (draw-asteroids)
  (end-drawing))

(engine/loop my-init update-state draw screen-width screen-height "Asteroids")
