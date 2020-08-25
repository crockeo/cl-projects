;;;; input.lisp implements a buffer-based input system where sequences of simple inputs can combine
;;;; input complex inputs.

;;; There are three levels of input in this input system:
;;;
;;; 1. Raw scancodes
;;;   These correspond to inputs on a particular input system, e.g. on a keyboard these are the
;;;   keys.
;;;
;;; 2. Simple inputs
;;;   These are inputs inferred directly from scancodes. There is a one-to-one mapping between a
;;;   particular 'chord' of scancode (multiple scancodes held down at once) and an input.
;;;
;;; 3. Complex inputs
;;;   These are inputs inferred from sequences of simple inputs.

(defvar all-scancodes
  '(:scancode-a
    :scancode-d
    :scancode-i
    :scancode-j
    :scancode-k
    :scancode-l
    :scancode-o
    :scancode-s
    :scancode-u
    :scancode-w))

(defvar default-input-map
  '(((:scancode-u) . :light-punch)
    ((:scancode-i) . :medium-punch)
    ((:scancode-o) . :heavy-punch)

    ((:scancode-j) . :light-kick)
    ((:scancode-k) . :medium-kick)
    ((:scancode-l) . :heavy-kick)

    ((:scancode-w :scancode-a) . :up-back)
    ((:scancode-w :scancode-d) . :up-forward)

    ((:scancode-s :scancode-a) . :down-back)
    ((:scancode-s :scancode-d) . :down-forward)

    ((:scancode-w) . :up)
    ((:scancode-s) . :down)
    ((:scancode-a) . :back)
    ((:scancode-d) . :forward)))

(defvar default-complex-input-trie
  (trie-from-alist
   '(((:down :down-forward :forward :light-punch) . :qcf-light-punch)
     ((:down :down-back :back :light-punch) . :qcb-light-punch))))

(defun get-input (scancode-chord)
  (assoc-if
   (lambda (key)
     (equal key scancode-chord))
   default-input-map))

(defun get-best-input (scancodes)
  (loop :for (scancode-chord . input) :in default-input-map
        :do
           (when (subset scancode-chord scancodes)
             (return input))))

(defun input-timestamp (input)
  "Gets the timestmap of an input in the input buffer."
  (first input))

(defun input-value (input)
  "Gets the value of an input in the input buffer."
  (second input))

(defun clear-stale-buffer-inputs (buffer max-delay last-timestamp)
  "Clears 'stale' buffer inputs, i.e. inputs that occurred at a maximum delay after some other
input."
  (decons buffer
    (let ((timestamp (input-timestamp head)))
      (cond
        ((null head) nil)
        ((> (- last-timestamp timestamp) max-delay) nil)
        (t (cons head (clear-stale-buffer-inputs
                       tail
                       max-delay
                       timestamp)))))))

(defun get-buffered-input (buffer)
  "Returns either a complex, buffered input, if the current buffered input is applicable, or
otherwise the most recent simple input."
  (let* ((input-sequence (mapcar #'input-value buffer))
         (complex-input (apply #'trie-get
                               default-complex-input-trie
                               (reverse input-sequence))))
    (if complex-input
        complex-input
        (car input-sequence))))
