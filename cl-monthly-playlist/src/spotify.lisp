(defclass spotify ()
  ((client-id
    :initform (read-file-as-string client-id-path))

   (client-secret
    :initform (read-file-as-string client-secret-path))

   (auth-code
    :initform nil)

   (access-token
    :initform nil)

   (expires-at
    :initform nil)

   (refresh-token
    :initform nil)))

(define-condition spotify-api-cond (error)
  ((name
    :initform nil
    :initarg :name
    :reader name)
   (description
    :initform nil
    :initarg :description
    :reader description)))

(defmethod print-object ((c spotify-api-cond) s)
  (format s "SPOTIFY-API-COND:~%  NAME: ~A~%  DESCRIPTION: ~A" (name c) (description c)))

(defun open-spotify-auth-page (client-id scopes)
  "Opens the spotify auth page in the system's browser."
  (trivial-open-browser:open-browser
   (make-url (list accounts-api-url "authorize")
             `(("client_id" . ,client-id)
               ("response_type" . "code")
               ("redirect_uri" . ,oauth-server-url)
               ("scope" . ,(when scopes (format nil "~{~A~^ ~}" scopes)))))))

(defun spotify-get-auth-code (spotify &key scopes)
  "Sends the user to the Spotify OAuth page to get an auth code."
  (with-slots (client-id auth-code) spotify
    (let ((oauth-server (make-oauth-server)))
      (open-spotify-auth-page client-id scopes)

      (bt-sem:wait-on-semaphore (slot-value oauth-server 'auth-code-ready))
      (setf auth-code (slot-value oauth-server 'auth-code))

      (oauth-server-stop oauth-server)

      spotify)))

(defmacro spotify-safe-request (res-name drakma-request &body body)
  "Performs a 'safe' request against the spotify API, by automatically wrapping JSON and API errors
in the spotify-api-cond."
  `(let* ((raw-response ,drakma-request)
          (,res-name (handler-case (parse-response raw-response)
                       (error (c)
                         (error 'spotify-api-cond
                                :name "json-cond"
                                :description (format nil "~A" c))))))
     (let ((error-name (assoc :error ,res-name))
           (error-desc (assoc :error--description ,res-name)))
       (when (or error-name error-desc)
         (error 'spotify-api-cond
                :name error-name
                :description error-desc)))

     ,@body))

(defun spotify-get-token (spotify)
  "Given the auth-code populated by spotify-authorize, create an access-token to interact withthe
API."
  (with-slots (client-id client-secret auth-code access-token expires-at refresh-token) spotify
    (spotify-safe-request
        res
        (drakma:http-request
         (url-path accounts-api-url "api" "token")
         :method :post
         :parameters `(("grant_type" . "authorization_code")
                       ("code" . ,auth-code)
                       ("redirect_uri" . ,oauth-server-url)
                       ("client_id" . ,client-id)
                       ("client_secret" . ,client-secret)))

      (let ((new-access-token (cdr (assoc :access--token res)))
            (expires-in (cdr (assoc :expires--in res)))
            (new-refresh-token (cdr (assoc :refresh--token res))))
        (setf access-token new-access-token)
        (setf expires-at (+ (current-time-seconds) expires-in))
        (setf refresh-token new-refresh-token))

      spotify)))

(defmacro define-spotify-endpoint (name args method url &optional params)
  "Defines a single wrapper around the Spotify API."
  (let ((additional-params ()))
    (when (and (eql method :get) params)
      (pushl `(:parameters ,params)
             additional-params))

    (when (and (not (eql method :get)) params)
      (pushl `(:content (serialize-body ,params))
             additional-params))

    `(defun ,(join-symbols 'spotify name) (client ,@args)
       (spotify-safe-request
           res
           (drakma:http-request
            (url-path ,api-url ,@url)
            :method ,method
            :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" (slot-value client 'access-token)))
                                  ("Content-Tyoe" . "application/json"))
            ,@additional-params)
         res))))

(defmacro define-spotify-endpoints (&body descriptors)
  `(progn
     ,@(mapcar
        (lambda (descriptor)
          (destructuring-bind (name args method url &optional params) descriptor
            `(define-spotify-endpoint ,name ,args ,method ,url ,params)))
        descriptors)))

(define-spotify-endpoints
  ;; Album API
  (get-album (id) :get ("albums" id))
  (get-album-tracks (id) :get ("albums" id "tracks"))

  ;; Artists API
  (get-artist (id) :get ("artists" id))
  (get-artist-albums (id) :get ("artists" id "albums"))
  (get-artist-top-tracks (id) :get ("artists" id "top-tracks"))
  (get-artist-related-artists (id) :get ("artists" id "related-artists"))

  ;; Playlist API
  (create-playlist (user-id name &key (public t) collaborative (description ""))
                   :post
                   ("users" user-id "playlists")
                   `(("name" . ,name)
                     ("public" . ,public)
                     ("collaborative" . ,collaborative)
                     ("description" . ,description)))

  (get-me-playlists () :get ("me" "playlists"))

  ;; Users API
  (get-me () :get ("me"))

  ;; TODO: rest of the endpoints
  )
