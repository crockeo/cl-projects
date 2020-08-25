(defvar *spotify-client* nil)

(defun create-and-auth-spotify-client (scopes)
  (unless *spotify-client*
    (setf *spotify-client* (make-instance 'spotify)))

  (spotify-get-auth-code *spotify-client* :scopes scopes)
  (spotify-get-token *spotify-client*))

(defun get-user-id ()
  (cdr (assoc :id (spotify-get-me *spotify-client*))))

(defun make-test-playlist (name)
  (spotify-create-playlist *spotify-client*
                           (get-user-id)
                           name))

(defun last-month-playlist-name ()
  "Gets the name of the last playlist."
  (multiple-value-bind (year month) (get-last-year-month)
    (format nil "~4,,,'0<~A~>-~2,,,'0<~A~>" year month)))

(defun main ()
  (create-and-auth-spotify-client '("playlist-modify-public"))
  (make-test-playlist "test playlist please ignore"))
