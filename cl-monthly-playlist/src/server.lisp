;;;; server.lisp

;;;
;;; This file implements a short-lived HTTP server used to capture the authorization code from
;;; Spotify's OAuth2 flow.
;;;

(defparameter oauth-server-host "localhost")
(defparameter oauth-server-port 8080)
(defparameter oauth-server-url
  (format nil "http://~A:~A" oauth-server-host oauth-server-port))

(defclass oauth-server (hunchentoot:acceptor)
  ((dispatch-table
    :initform nil)

   (auth-code
    :initform nil)

   (auth-code-ready
    :initform (bt-sem:make-semaphore :count 0)))

  (:default-initargs
   :address "localhost"))

(defmethod hunchentoot:acceptor-dispatch-request ((oauth-server oauth-server) request)
  "Dynamically dispatches a request to the appropriate handler."
  (with-slots (dispatch-table) oauth-server
    (dolist (dispatcher dispatch-table)
      (let ((handler (funcall dispatcher request)))
        (when handler
          (return-from hunchentoot:acceptor-dispatch-request (funcall handler))))))
  (call-next-method))

(defmethod oauth-server-stop ((oauth-server oauth-server))
  "Stops the oauth server."
  (hunchentoot:stop oauth-server))

(defun get-param (name alist)
  "For some reason I couldn't get assoc to work on the GET params normally, so I just made this."
  (cdr (assoc-if (lambda (x) (equal x name)) alist)))

(defun make-oauth-handler (oauth-server)
  (lambda ()
    (with-slots (auth-code auth-code-ready) oauth-server
      (let ((code (get-param "code" (hunchentoot:get-parameters*))))
        (when code
          (setf auth-code code)
          (bt-sem:signal-semaphore auth-code-ready 1))

        (read-file-as-string "www/confirm-screen.html")))))

(defun make-oauth-server ()
  "Constructs a new oauth server that accepts and stores the provided code."
  (let ((oauth-server (make-instance 'oauth-server :port oauth-server-port)))
    (with-slots (dispatch-table) oauth-server
      (push
       (hunchentoot:create-prefix-dispatcher "/" (make-oauth-handler oauth-server))
       dispatch-table)

      (hunchentoot:start oauth-server)

      oauth-server)))
