;;;; trie.lisp implements a Trie data structure.
;;;;
;;;; https://en.wikipedia.org/wiki/Trie

;;; Each cell if a tree is a list of form:
;;;
;;;   (value (children))
;;;
;;; A nil value means there is nothing contained in that individual node. Nil children means that
;;; the node is terminal.

(defclass trie ()
  ((key
    :initarg :key)

   (value
    :initarg :value
    :initform nil)

   (children
    :initarg :children
    :initform nil)))

(defmethod print-object ((obj trie) s)
  (with-slots (key value children) obj
    (format s "~A ~A ~{~%~A~}" key value children)))

(defun trie-subtrie (trie key)
  "Gets an individual subtrie of a trie."
  (find-if
   (lambda (subtrie)
     (equal (slot-value subtrie 'key) key))
   (slot-value trie 'children)))

(defun trie-get (trie &rest keys)
  "Gets an item from a trie at a provided series of keys."
  (cond
    ((null trie) nil)
    ((null keys) (slot-value trie 'value))
    (t (apply #'trie-get (trie-subtrie trie (car keys)) (cdr keys)))))

(defun trie-add (trie value &rest path)
  "Adds a value to the trie. This modifies the trie that is passed in."
  (decons path
    (if (null head)
        (setf (slot-value trie 'value) value)
        (let ((subtrie (trie-subtrie trie head)))
          (unless subtrie
            (setf subtrie (make-instance 'trie :key head))
            (push subtrie (slot-value trie 'children)))
          (apply #'trie-add (trie-subtrie trie head) value tail))))
  trie)

(defun trie-from-alist (alist)
  "Constructs a trie from an alist."
  (let ((trie-instance (make-instance 'trie :key nil)))
    (loop for entry in alist do
      (let ((path (car entry))
            (value (cdr entry)))
        (apply #' trie-add trie-instance value path)))
    trie-instance))
