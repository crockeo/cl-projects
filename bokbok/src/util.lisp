(defun subset (s1 s2)
  "Determines if s1 is a subset of s2."
  (declare
   (type list s1)
   (type list s2))
  (= (length s1)
     (length (intersection s1 s2))))

(defmacro decons (l &body body)
  "Fun little macro that splits up a list into a head (car l) and a tail (cdr l) and binds them for
the body. This probably exists already in the stdlib but I couldn't find it :)"
  `(let* ((l-value ,l)
          (head (car l-value))
          (tail (cdr l-value)))
     ,@body))

(defun tree-map (xs func)
  "Maps a function over a tree."
  (loop for x in xs
        collect (if (listp x)
                    (tree-map x func)
                    (funcall func x))))

(defmacro rewrite (name-value &body body)
  "Rewrites a name to a value whenever it occurs in the body of this macro."
  (destructuring-bind (name value) name-value
    `(progn
       ,@(tree-map
          body
          (lambda (e)
            (if (eq e name)
                value
                e))))))

(defun lerp (val target seconds dt)
  "Linearly interpolates between a value and its target value over the course of seconds. The
argument dt is used to represent the passage of time."
  (+ val (* (- target val) (/ 1 seconds) dt)))

(defun now-seconds ()
  "Returns the current time in seconds (up to the resolution provided by
internal-time-units-per-second)."
  (/ (get-internal-real-time)
     float-time-units-per-second))

(defun hsv-to-rgb (h s v)
  "Converts HSV values to RGB values."
  (let* ((h (mod h 360))
         (c (* s v))
         (x (* c (- 1 (abs (- (mod (/ h 60) 2) 1)))))
         (m (- v c)))
    (multiple-value-bind (r g b)
        (cond
          ((and (<= 0 h) (< h 60)) (values c x 0))
          ((and (<= 60 h) (< h 120)) (values x c 0))
          ((and (<= 120 h) (< h 180)) (values 0 c x))
          ((and (<= 180 h) (< h 240)) (values 0 x c))
          ((and (<= 240 h) (< h 300)) (values X 0 c))
          ((and (<= 300 h) (< h 360)) (values c 0 x)))
      (values
       (round (* (+ r m) 255))
       (round (* (+ g m) 255))
       (round (* (+ b m) 255))))))
