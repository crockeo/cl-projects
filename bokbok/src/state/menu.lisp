(defclass menu ()
  ())

(defmethod state-load ((m menu) renderer))

(defmethod state-destroy ((m menu)))

(defmethod state-render ((m menu) renderer))

(defmethod state-update ((m menu) state-stacks input dt))
