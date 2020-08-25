(defvar *player-max-speed* 500)
(defvar *player-acceleration-seconds* 0.25)
(defvar *player-deceleration-seconds* 0.1)
(defvar *player-anim-rate* 0.05)

(defclass player ()
  ((x :initarg :x)
   (y :initarg :y)

   (vx :initform 0)
   (vy :initform 0)

   (w :initarg :w)
   (h :initarg :h)

   (animations :initform nil)
   (current-anim :initform nil)
   (anim :initform nil)))

(defun frame-paths (min-frame max-frame)
  "Generates frame paths for a given min/max range."
  (loop for x from min-frame to max-frame
        collect (format nil "res/02_Ryu/~5,,,'0<~A~>.png" x)))

(defmethod player-load ((obj player) renderer)
  "Loads resources necessary for the player."
  (with-slots (animations current-anim) obj
    ;; TODO: find a way to do this that can be re-run
    (setf animations
          (load-animations
           renderer
           `((:name :idle
              :paths ,(frame-paths 2593 2602)
              :tick ,*player-anim-rate*
              :should-loop t)

             (:name :light-punch
              :paths ,(frame-paths 2944 2948)
              :tick ,*player-anim-rate*
              :should-loop nil)

             (:name :medium-punch
              :paths ,(frame-paths 2949 2955)
              :tick ,*player-anim-rate*
              :should-loop nil)

             (:name :heavy-punch
              :paths ,(frame-paths 2956 2966)
              :tick ,*player-anim-rate*
              :should-loop nil)

             (:name :qcf-light-punch
              :paths ,(frame-paths 3265 3275)
              :tick ,*player-anim-rate*
              :should-loop nil))))

    (setf current-anim (gethash :idle animations))))

(defmethod player-render ((obj player) renderer)
  "Renders the player onto the screen."
  (with-slots (x y w h current-anim) obj
    (sprite-render current-anim
                   renderer
                   x y
                   w h)))

(defmethod player-set-animation ((obj player) animation-name)
  "Sets the animation for the player to some previously loaded animation."
  (with-slots (animations current-anim) obj
    (setf current-anim (gethash animation-name animations))
    (with-slots (tick next-tick current-frame) current-anim
      (setf next-tick tick)
      (setf current-frame 0))))

(defmethod player-update ((obj player) input dt)
  "Updates the player's state, including the ticking of the animation."
  (with-slots (x y vx vy animations current-anim) obj
    (animation-tick current-anim dt)

    (when (animation-done current-anim)
      (player-set-animation obj :idle))

    ;; TODO: come up with a better way to determine that a player is interruptable / can change
    ;; animation
    (when (and (eql current-anim (gethash :idle animations))
               (gethash input animations))
      (player-set-animation obj input))

    (multiple-value-bind (dx dy)
        (case input
          (:up (values 0 -1))
          (:down (values 0 1))
          (:back (values -1 0))
          (:forward (values 1 0))

          (:up-back (values -1 -1))
          (:up-forward (values 1 -1))

          (:down-back (values -1 1))
          (:down-forward (values 1 1)))
      (if (and dx dy)
          (progn
            (setf vx (lerp vx (* dx *player-max-speed*) *player-acceleration-seconds* dt))
            (setf vy (lerp vy (* dy *player-max-speed*) *player-acceleration-seconds* dt)))
          (progn
            (setf vx (- vx (* vx (/ 1 *player-deceleration-seconds*) dt)))
            (setf vy (- vy (* vy (/ 1 *player-deceleration-seconds*) dt))))))

    (setf x (+ x (* vx dt)))
    (setf y (+ y (* vy dt)))))
