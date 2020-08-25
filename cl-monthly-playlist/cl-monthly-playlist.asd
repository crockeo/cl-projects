(defsystem "cl-monthly-playlist"
  :description "Managing Spotify playlists with Common Lisp."
  :version "0.0.1"
  :author "Cerek Hillen <cerekh@gmail.com>"
  :license "MIT"
  :depends-on ("bt-semaphore" "cl-json" "drakma" "hunchentoot" "trivial-open-browser")
  :components ((:module
                :src
                :components ((:file "main"
                              :depends-on ("spotify" "util"))
                             (:file "server"
                              :depends-on ("util"))
                             (:file "spotify"
                              :depends-on ("server" "spotify-constants" "util"))
                             (:file "spotify-constants")
                             (:file "spotify-models")
                             (:file "util")))))
