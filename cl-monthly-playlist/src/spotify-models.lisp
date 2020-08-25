;;;; spotify-models.lisp

;;;
;;; This file defines tooling to construct the object models described in Spotify's documentation:
;;;
;;;   https://developer.spotify.com/documentation/web-api/reference/object-model/
;;;
;;; For each object we define a class (define-spotify-object-model-class) and an initializer
;;; (define-spotify-object-model-initializer). The latter takes parsed JSON as structured by cl-json
;;; and initializes a class's fields.
;;;

(defmacro define-spotify-object-model-class (name fields)
  "Defines a CLOS wrapper around a Spotify object model."
  `(defclass ,(join-symbols 'spotify name) ()
     (,@(loop for (field-name . _) in fields
              collect `(,field-name
                        :initarg ,(symbol-keyword field-name))))))

(defmacro define-spotify-object-model-initializer (name fields)
  "Defines the initializer for a Spotify object model, so it can be constructed from parsed JSON."
  `(defmethod initialize-instance :after ((om ,(join-symbols 'spotify name)) &key parsed-json)
     ,@(loop for (field-name . parser) in fields
             collect `(let ((value (cdr (assoc ,(symbol-keyword field-name) parsed-json))))
                        (when value
                          (setf (slot-value om (quote ,field-name)) (funcall ,parser value)))))))

(defmacro define-spotify-object-model (name &body fields)
  "Defines a spotify object model and an accompanying parser."
  `(progn
     (define-spotify-object-model-class ,name ,fields)
     (define-spotify-object-model-initializer ,name ,fields)))

;;;
;;; To support nested structures, we execute a secondary parser against the contents of each field
;;; as described by a parser function provided to the macro.
;;;
;;; We also provide tooling (om-parser) to make the appropriate function signature for parsing
;;; nested object models.
;;;

(defmacro om-parser (name)
  "Constructs a function of the appropriate type signature to parse an object model within an object
model definition."
  `#'(lambda (parsed-json)
       (make-instance (quote ,(join-symbols 'spotify name))
                      :parsed-json parsed-json)))

(defun null-parser (x)
  "Identity function for things that don't need to be parsed."
  x)

;;;
;;; Model definitions
;;;

(define-spotify-object-model playlist
  (id . #'null-parser)
  (name . #'null-parser))
