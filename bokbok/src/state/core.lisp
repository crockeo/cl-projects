;;;; state/core.lisp

;;;
;;; Defines the structure of states in bokbok.
;;;
;;; States have 4 lifecycle methods, that are called while executing the program:
;;;


;; Executed when a state is pushed onto a state stack. Loads resources necessary for that state.
;; This function should be idempotent, as it will be re-run during development.
(defgeneric state-load (state renderer))

;; Executed when a state is removed from the stack. Frees C resources allocated.
(defgeneric state-destroy (state))

;; Executed every tick. Renders the state onto the screen.
(defgeneric state-render (state renderer))

;; Executed every tick. Updates the state with the provided input and time delta. This function is
;; also provided with the parent state-stacks, so that it can change the state being executed.
(defgeneric state-update (state state-stacks input dt))

;;;
;;; Layered states are represented by a stack. States can be pushed onto and popped from the stack,
;;; dynamically changing which states are executed.
;;;
;;; States can also choose to intervene in other states' execution by returning a nil value.
;;;

(defclass state-stack ()
  ((states
    :initform nil)))

(defmethod push-state ((s state-stack) state)
  "Pushes a new state onto the state stack."
  (with-slots (states) s
    (push state states)))

(defmethod pop-state ((s state-stack))
  "Pops a state from the state stack."
  (with-slots (states) s
    (pop states)))

(defmethod set-state ((s state-stack) state)
  "Replaces the state stack with a single state."
  (with-slots (states) s
    (setf states nil)
    (push state states)))

;;;
;;; The state stack is a state itself, and can be added as a state. Note in the `update` function
;;; that a state-stack passes to its children a list of all state stacks, including itsef.
;;;

(defmethod state-load ((s state-stack) renderer))
(defmethod state-destroy ((s state-stack)))

(defmethod state-render ((s state-stack) renderer)
  (with-slots (states) s
    (dolist (state states)
      (unless (state-render state renderer)
        (return-from state-render)))))

(defmethod state-update ((s state-stack) state-stacks input dt)
  (declare (type state-stacks list)
           (type input keyword)
           (type dt number))

  (with-slots (states) s
    (dolist (state states)
      (unless (update state (cons s state-stacks) input dt)
        (return-from state-update)))))
