(mapcar
 #'ql:quickload
 '(:cl-annot
   :drakma
   :trivial-dump-core))

(require "asdf")
(asdf:load-system :sta)

(defun save-image ()
  (trivial-dump-core:save-executable "sta"
                                     #'main))
