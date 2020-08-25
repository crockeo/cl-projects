(defclass game ()
  ())

(defmethod state-load ((g game) renderer))

(defmethod state-destroy ((g game)))

(defmethod state-render ((g game) renderer))

(defmethod state-update ((g game) state-stacks input dt))
