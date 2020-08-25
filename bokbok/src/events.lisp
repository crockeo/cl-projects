(defvar event-types
  '(
    (:idle . :nil)
    (:windowevent . :window)
    (:keydown . :key)
    (:keyup . :key)
    (:mousemotion . :motion)
    (:mousebuttondown . :button)
    (:mousebuttonup . :button)
    (:mousewheel . :wheel)

    ;; Remaining events:

    ;; '((:controlleraxismotion . :caxis)
    ;;   (:controllerbuttondown . :cbutton)
    ;;   (:controllerbuttonup . :cbutton)
    ;;   (:controllerdeviceadded . :cdevice)
    ;;   (:controllerdeviceremapped . :cdevice)
    ;;   (:controllerdeviceremoved . :cdevice)
    ;;   (:dropfile . :drop)
    ;;   (:joyaxismotion . :jaxis)
    ;;   (:joyballmotion . :jball)
    ;;   (:joybuttondown . :jbutton)
    ;;   (:joybuttonup . :jbutton)
    ;;   (:joydeviceadded . :jdevice)
    ;;   (:joydeviceremoved . :jdevice)
    ;;   (:joyhatmotion . :jhat)
    ;;   (:syswmevent . :syswm)
    ;;   (:textediting :edit)
    ;;   (:textinput :text)
    ;;   (:userevent . :user)
    ;;   (:lisp-message . :user)
    ))


(defvar event-fields
  ;; For more: https://wiki.libsdl.org/SDL_Event
  '((:window . (:type :timestamp :window-id :event :data1 :data2))
    (:key . (:type :timestamp :window-id :state :repeat :keysym))
    (:motion . (:type :timestamp :window-id :which :state :x :y :xrel :yrel))
    (:button . (:type :timestamp :window-id :which :button :state :clicks :x :y))
    (:wheel . (:type :timestamp :window-id :which :x :y :direction))
    (:nil . ())
    ))

(defun get-event-fields (event-type)
  "Gets the fields of a particular event type."
  (cdr
   (assoc
    (cdr (assoc event-type event-types))
    event-fields)))

(defun get-event-binders (event-type)
  "Constructs the binding form for sdl2:with-event-loop event handlers, used to forward events to a
custom event handler that can be re-evaluated at runtime."
  (mapcar
   (lambda (event-field)
     (list event-field (intern (symbol-name event-field))))
   (get-event-fields event-type)))

;; TODO: figure out how to extract values from this and forward it to an event handler
(defmacro forward-to-event-handler (forward-args event-handler)
  `(sdl2:with-event-loop (:method :poll)
     ,@(mapcar
        (lambda (event-type-pair)
          (let* ((event-type (car event-type-pair))
                 (binders (get-event-binders event-type)))
            `(,event-type
              ,(apply #'append binders)
              (,event-handler ,@forward-args ,event-type ,@(mapcar (lambda (binder) (second binder)) binders))
              )))
        event-types)
     (:quit () t)))
