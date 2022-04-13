#!/usr/bin/env janet
(use jaylib)

(def screen-width 600)
(def screen-height 600)
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
  (* (asteroid :size) 15))

(defn make-asteroid [x y size velocity]
  @{:size size
    :coords [x y]
    :velocity velocity})

(defn random-asteroid-velocity []
  [(* 3 (- (math/random) 0.5))
   (* 3 (- (math/random) 0.5))])

(defn spawn-asteroid []
  (make-asteroid
   (* screen-width (math/random))
   (* screen-height (math/random))
   3
   (random-asteroid-velocity)))

(defn make-ship [width height]
  @{:size 25
  :position [(/ width 2) (/ height 2)]
  :orientation 0.0
  :velocity [0.0 0.0]})

(defn make-state [width height]
  (let [canvas (gen-image-color width height :ray-white)]
    @{:screen-width width
      :screen-height height
      :canvas canvas
      :texture (load-texture-from-image canvas)
      :ship (make-ship width height)
      :asteroids [(spawn-asteroid)]}))

(defn set-state [key value]
  (set (state key) value))

(defn get-state [key]
  (state key))

(defn my-init []
  (set state (make-state screen-width screen-height)))

(defn vector-add [v1 v2]
  [(+ (get v1 0) (get v2 0)) (+ (get v1 1) (get v2 1))])

(defn calculate-thrust-vector []
  [(* (math/cos ((state :ship) :orientation)) 0.05)
   (* (math/sin ((state :ship) :orientation)) 0.05)])

(defn calculate-ship-velocity []
  (vector-add ((state :ship) :velocity) (calculate-thrust-vector)))

(defn vector-wrap [v modulus]
  [(if (< (v 0) 0)
     (+ 600 (v 0))
     (% (math/abs (v 0)) modulus))
   (if (< (v 1) 0)
     (+ 600 (v 1))
     (% (math/abs (v 1)) modulus))])

(defn move-ship []
  (let [ship (state :ship)]
    (set (ship :position)
               (vector-wrap (vector-add (ship :position) (ship :velocity)) screen-width))))

(defn move-asteroid [asteroid]
    (set (asteroid :coords)
        (vector-wrap (vector-add (asteroid :coords) (asteroid :velocity)) (state :screen-width))))

(defn move-asteroids []
    (loop [asteroid :in (state :asteroids)]
        (move-asteroid asteroid)))

(defn point-distance [a b]
  (math/sqrt
    (+
      (math/pow (- (a 0) (b 0)) 2)
      (math/pow (- (a 1) (b 1)) 2))))

(defn point-in-circle? [point circle]
  (let [distance (point-distance point (circle :center))
        radius (circle :radius)]
    (< distance radius)))

(defn find-ship-center []
  (let [ship (state :ship)
        ship-position (ship :position)
        ship-x (ship-position 0)
        ship-y (ship-position 1)
        ship-size (ship :size)]
    [(/ (+ ship-x ship-x (+ ship-x ship-size)) 3)
    (/ (+ ship-y ship-y (+ ship-y ship-size)) 3)]))

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
    p3 (vector-add (rotate-point [(+ ship-x (+ ship-size)) ship-y]) ship-center)]
  [p1 p2 p3]))

(defn asteroid-collides-ship? [asteroid]
  (let [circle @{:center (asteroid :coords) :radius (asteroid-radius asteroid)}]
    (any? (map (fn [point] (point-in-circle? point circle)) (ship-points)))))

(defn ship-collides? []
  (any? (map asteroid-collides-ship? (state :asteroids))))

(defn handle-ship-collisions []
  (if (ship-collides?)
    (os/exit 1)))

(defn handle-keyboard-input []
  (let [ship (state :ship)]
    (if (key-down? :left) (set (ship :orientation) (- (ship :orientation) 0.05)))
    (if (key-down? :right) (set (ship :orientation) (+ (ship :orientation) 0.05)))
    (if (key-down? :up) (set (ship :velocity) (calculate-ship-velocity)))))

(defn my-update []
  (handle-keyboard-input)
  (move-ship)
  (move-asteroids)
  (handle-ship-collisions))

(defn draw-asteroid [asteroid]
  (draw-circle
    (splice (map math/floor (asteroid :coords)))
    (asteroid-radius asteroid)
    :ray-white))

(defn draw-asteroids []
  (loop [asteroid :in (state :asteroids)]
    (draw-asteroid asteroid)))

(defn draw-ship [state]
  (draw-triangle (splice (ship-points)) :ray-white))

(defn spawn-asteroid [])

(defn my-draw []
  (begin-drawing)
  (clear-background :black)
  (draw-ship state)
  (draw-asteroids)
  (end-drawing))

(engine/loop my-init my-update my-draw screen-width screen-height "Asteroids")
