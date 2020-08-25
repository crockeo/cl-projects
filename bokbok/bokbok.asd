(defsystem "bokbok"
  :description "A fighting game!"
  :version "0.0.1"
  :author "Cerek Hillen <cerekh@gmail.com>"
  :license "MIT"
  :depends-on ("bt-semaphore" "sdl2" "sdl2-image" "swank")
  :components ((:module
                :src
                :components ((:file "constants")

                             (:file "events")

                             (:file "input"
                              :depends-on ("trie" "util"))

                             (:file "main"
                              :depends-on ("constants" "events" "input" "player" "sprite"))

                             (:file "player"
                              :depends-on ("sprite"))

                             (:file "sprite"
                              :depends-on ("util"))

                             (:file "trie"
                              :depends-on ("util"))

                             (:file "util")

                             (:module
                              :state
                              :components ((:file "core")
                                           (:file "game"
                                            :depends-on ("core"))
                                           (:file "menu"
                                            :depends-on ("core"))))))))
