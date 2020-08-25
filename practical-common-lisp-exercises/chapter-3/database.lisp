(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push cd *db*))

(defun clear-db ()
  (setq *db* nil))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~A:~8T~A~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~A: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (unless (y-or-n-p "Another? [y/n]: ") (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setq *db* (read in)))))

(defmacro where (&rest clauses)
  `(lambda (cd)
     (and
      ,@(loop :while clauses
              :collect `(equal (getf cd ,(pop clauses)) ,(pop clauses))))))

(defun select (selector)
  (remove-if-not selector *db*))

(defmacro update (selector &rest clauses)
  `(setq *db*
         (mapcar
          (lambda (cd)
            (when (funcall ,selector cd)
              ,@(loop :while clauses
                      :collect `(setf (getf cd ,(pop clauses)) ,(pop clauses))))
            cd)
          *db*)))

(defun delete-rows (selector)
  (setq *db*
        (remove-if selector *db*)))
