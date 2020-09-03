(defpackage :main
  (:use :common-lisp))

(defmacro pkg (name)
  `(progn
     (in-package ,name)
     (annot:enable-annot-syntax)))
