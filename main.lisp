(require :cl-raylib)

(defpackage :raylib-user
 (:use :cl :raylib))

(in-package :raylib-user)



(defun main ()
 (let* ((screen-width 800)
       (screen-height 450)
       (ball-position (make-vector2 :x -100.0 :y -100.0))
       (ball-color +darkblue+)
       (ballz (make-array 1024 :fill-pointer 0))
       (canvas (gen-image-color screen-width screen-height +white+)))
   (with-window (screen-width screen-height "raylib [core] example - mouse input")
                (set-target-fps 60) ; Set our game to run at 60 FPS
                (loop
                  (if (window-should-close) (return)) ; dectect window close button or ESC key
                  (setf ball-position (get-mouse-position))
                  (print ball-position)
                  (if (is-mouse-button-down +mouse-left-button+) (vector-push-extend (get-mouse-position) ballz))
                  ; (cond
                   ; ((is-mouse-button-down +mouse-left-button+)
                   ;  (vector-push-extend (get-mouse-position) ballz)
                   ;  (setf ball-color +maroon+))
                   ; ((is-mouse-button-pressed +mouse-middle-button+)
                   ;  (setf ball-color +lime+))
                   ; ((is-mouse-button-pressed +mouse-right-button+)
                   ;  (setf ball-color +darkblue+)))

                  (with-drawing
                    (clear-background +raywhite+)
                    (loop
                        if (length ballz) > 100
                    )
                    (loop for coord being the elements of ballz do (draw-circle-v coord 12.0 ball-color))
                    (draw-circle-v ball-position 12.0 ball-color)
                    (draw-text "move ball with mouse and click mouse button to change color" 10 10 20 +darkgreen+)
                    (draw-fps 10 40))))))


 (main)
