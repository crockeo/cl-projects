(defun url-path (&rest parts)
  (format nil "~{~A~^/~}" parts))

(defun get-param-pair (key-value)
  (destructuring-bind (key . value) key-value
    (format nil "~A=~A" key value)))

(defun get-params (key-values)
  (format nil
          "~{~A~^&~}"
          (loop for key-value in key-values
                unless (null (cdr key-value))
                  collect (get-param-pair key-value))))

(defun make-url (parts &optional key-values)
  (with-open-stream (s (make-string-output-stream))
    (format s "~A" (apply #'url-path parts))
    (when key-values
      (format s "?~A" (get-params key-values)))

    (get-output-stream-string s)))

(defun read-file-as-string (path)
  "Reads a file as a string, trimming off any final newlines."
  (with-open-file (f path)
    (let ((contents (make-string (file-length f))))
      (read-sequence contents f)
      (string-trim '(#\newline #\linefeed) contents))))

(defun write-string-to-file (path contents)
  "Writes the contents of a string to a file."
  (with-open-file (f path
                     :direction :output
                     :if-does-not-exist :create)
    (write-sequence contents f)))

(defun parse-response (res)
  "Parses a drakma response (that's in JSON) into a JSON object."
  (with-input-from-string (in (map 'string #'code-char res))
    (json:decode-json in)))

(defun serialize-body (req)
  "Serializes Common Lisp data to a JSON string."
  (with-output-to-string (out)
    (json:encode-json req out)))

(defun current-time-seconds ()
  "Gets the current time in seconds."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun join-symbols (&rest symbols)
  "Joins symbols together creating a new, hyphenated symbol."
  (intern (format nil
                  "~{~A~^-~}"
                  (mapcar #'symbol-name symbols))))

(defun symbol-keyword (symbol)
  "Interns a the name of a symbol as a keyword."
  (intern (symbol-name symbol) "KEYWORD"))

(defmacro pushl (list place)
  "Pushes a list of things into a place."
  `(let ((reversed (reverse ,list)))
     (loop for x in reversed
           do (push x ,place))))

(defun get-last-year-month ()
  "I don't actually know if this is wrong, but I don't trust myself to make datetime code. Proceed
with caution."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second minute hour date))
    (decf month)
    (when (= month 0)
      (setf month 12)
      (decf year))

    (values year month)))
