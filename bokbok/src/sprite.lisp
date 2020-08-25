(defvar *sprite-store* (make-hash-table))

(defclass sprite ()
  ((texture
    :initarg :texture)))

(defmethod sprite-render ((obj sprite) renderer x y w h)
  "Renders a sprite at a given coordinate pair (x, y), and a given size (w, h)."
  (sdl2:render-copy renderer
                    (slot-value obj 'texture)
                    :dest-rect (sdl2:make-rect (round x) (round y) (round w) (round h))))

(defun load-sprite (renderer path)
  "Loads an image from a file, constructs an appropriate texture, and attaches it to a sprite."
  (let ((surface nil)
        (texture nil)
        (sprite nil))
    (setf surface (sdl2-image:load-image path))
    (when (null surface)
      (signal 'load-sprite :message "null surface"))

    (setf texture (sdl2:create-texture-from-surface renderer surface))
    (when (null texture)
      (signal 'load-sprite :message "null texture"))

    (sdl2:free-surface surface)
    (setf sprite (make-instance 'sprite :texture texture))
    (finalize sprite
              (lambda ()
                (sdl2:destroy-texture texture)))

    sprite))

(defun get-sprite (renderer path)
  "Loads a sprite from disk with caching."
  (rewrite (sprite (gethash path *sprite-store*))
    (unless sprite
      (setf sprite (load-sprite renderer path)))
    sprite))

(defun get-sprites (renderer paths)
  "Loads a series of sprites."
  (loop for path in paths
        collect (get-sprite renderer path)))

(defclass animation ()
  ((sprites
    :initarg :sprites)
   (tick
    :initarg :tick)
   (next-tick
    :initarg :next-tick)
   (current-frame
    :initform 0)
   (should-loop
    :initarg :should-loop
    :initform t)))

(defmethod sprite-render ((obj animation) renderer x y w h)
  "Renders the current sprite of an animation at a coordinate pair (x, y) at a given size (w, h)."
  (with-slots (sprites current-frame) obj
    (sprite-render (nth current-frame sprites) renderer x y w h)))

(defmethod animation-tick ((obj animation) dt)
  "Ticks an animation with a given delta time, moving between frames and optionally looping."
  (when (animation-done obj)
    (return-from animation-tick))

  (with-slots (sprites tick next-tick current-frame should-loop) obj
    (setf next-tick (- next-tick dt))
    (when (<= next-tick 0)
      (setf next-tick tick)
      (setf current-frame (mod (+ current-frame 1)
                               (length sprites))))))

(defmethod animation-done ((obj animation))
  "Returns true iff the animation provided has completed (implicitly that it should not loop)."
  (with-slots (sprites current-frame should-loop) obj
    (and (not should-loop)
         (= current-frame (- (length sprites) 1)))))

(defun load-animation (renderer paths tick &key (should-loop t))
  "Constructs a new animation from a series of paths and a tick rate."
  (make-instance 'animation
                 :sprites (get-sprites renderer paths)
                 :tick tick
                 :next-tick tick
                 :should-loop should-loop))

(defun load-animations (renderer animation-descriptors)
  (let ((animations (make-hash-table)))
    (dolist (descriptor animation-descriptors)
      (destructuring-bind (&key name paths tick should-loop) descriptor
        (setf (gethash name animations)
              (load-animation renderer
                              paths
                              tick
                              :should-loop should-loop))))

    animations))
