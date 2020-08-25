;; Load dependencies, just in case :)
(mapcar
 #'ql:quickload
 '(:bt-semaphore
   :cl-json
   :drakma
   :hunchentoot
   :trivial-open-browser))

;; Load the system
(require "asdf")
(asdf:load-system :cl-monthly-playlist)

(defun save-image ()
  (sb-ext:save-lisp-and-die "cl-monthly-playlist"
                            :toplevel #'main
                            :executable t
                            :purify t))
