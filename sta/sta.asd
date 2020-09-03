(defsystem "sta"
  :description "See Twitter As other people!"
  :version "0.0.1"
  :author "Cerek Hillen <cerekh@gmail.com>"
  :license "MIT"
  :depends-on ("cl-annot"
               "drakma"
               "trivial-dump-core")
  :components ((:module
                :src
                :components
                ((:file "packages")
                 (:file "main"
                  :depends-on ("packages"))))))
