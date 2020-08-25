(defvar *max-buffer-length* 10)
(defvar *max-buffer-delay-milliseconds* 100)

(defclass game-state ()
  ((win :initarg :win)
   (renderer :initarg :renderer)

   (player :initarg :player)

   (keyboard-state
    :initform nil)

   (input-buffer
    :initform nil)

   (last-time
    :initform (now-seconds))))

(defun render-quad (renderer size)
  (multiple-value-bind
        (max-x max-y) (sdl2:get-renderer-output-size renderer)
    (sdl2:render-fill-rect
     renderer
     (sdl2:make-rect (round (- (/ max-x 2) (/ size 2)))
                     (round (- (/ max-y 2) (/ size 2)))
                     (round size)
                     (round size)))))

(defun render (game-state)
  "Renders the current game state."
  (with-slots (renderer player) game-state
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-clear renderer)

    (let ((count 30))
      (loop :for i :from count :downto 0
            :collect (multiple-value-bind (r g b) (hsv-to-rgb (* 20 i) 0.7 1.0)
                       (sdl2:set-render-draw-color renderer r g b 255)
                       (render-quad renderer
                                    (* (sqrt 2) 50 i)))))

    (player-render player renderer)

    (sdl2:render-present renderer)))

(defun update (game-state dt)
  "Moves the little square around the screen."
  (with-slots (keyboard-state input-buffer player) game-state
    (let ((input (get-best-input keyboard-state))
          (now (sdl2:get-ticks)))
      ;; Populate the buffer based on which inputs are provided
      (when input
        (when (equal input (input-value (car input-buffer)))
          (pop input-buffer))
        (push (list now input) input-buffer))

      ;; Remove stale inputs from input-buffer
      (setf input-buffer
            (clear-stale-buffer-inputs input-buffer
                                       *max-buffer-delay-milliseconds*
                                       now)))

    (player-update player
                   (get-buffered-input input-buffer)
                   dt)))

(defun handle-idle (game-state)
  "Occurs whenever SDL is idle. Used to (1) repaint the screen, and (2) update the game state
per-tick."
  (with-slots (last-time) game-state
    (let* ((begin-loop-time (now-seconds))
           (dt (- begin-loop-time last-time)))

      (update game-state dt)
      (render game-state)

      (let ((end-loop-time (now-seconds)))
        (setf last-time end-loop-time)
        (sleep (- (/ 1 60) end-loop-time (- begin-loop-time)))))))

(defun handle-key (game-state type timestamp window-id state repeat keysym)
  "Occurs whenever a key is pressed or released. Manages the game's keyboard state."
  (declare (ignore type)
           (ignore timestamp)
           (ignore window-id))

  (when (= repeat 1)
    (return-from handle-key))

  (with-slots (keyboard-state) game-state
    (setf keyboard-state
          (funcall (if (= state 1) #'adjoin #'remove)
                   (sdl2:scancode keysym)
                   keyboard-state))))

(defun handle-event (game-state event-type &rest args)
  "Single pinchpoint at which we can re-evaluate all of its called functions. Used to edit the
application on the fly, which is harder to perform when called directly within the
sdl:with-event-loop macro."
  (let ((fn (case event-type
              (:idle #'handle-idle)
              (:keydown #'handle-key)
              (:keyup #'handle-key))))
    (when fn
      (handler-case (apply fn game-state args)
        (error (c)
          (with-open-stream (s (make-string-output-stream))
            (format s "~A" c)
            (print (get-output-stream-string s)))
          (sleep (/ 1 60)))))))

(defun run-game ()
  "Running the game."
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (let ((game-state (make-instance 'game-state
                                         :win win
                                         :renderer renderer
                                         :player (make-instance 'player
                                                                :x 10 :y 10
                                                                :w 300 :h 200))))
          (player-load (slot-value game-state 'player) renderer)
          (forward-to-event-handler (game-state) handle-event))))))

(defun start-swank-raw (emacs-port)
  (swank:stop-server emacs-port)
  (swank:create-server :port emacs-port
                       :dont-close t))

(defun start-swank-safe (emacs-port)
  (handler-case (start-swank-raw emacs-port)
    (error (c)
      (with-open-stream (s (make-string-output-stream))
        (format s "~A" c)
        (print (get-output-stream-string s)))
      (start-swank-safe emacs-port))))

(defun start-swank (emacs-port)
  (bt:make-thread
   (lambda ()
     (start-swank-safe emacs-port))))

(defun main ()
  (sdl2:make-this-thread-main #'run-game))

(defun main-with-swank ()
  (start-swank 4005)
  (main))
