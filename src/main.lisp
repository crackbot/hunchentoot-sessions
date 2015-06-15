
(in-package :hunchentoot-sessions)

;; Docs
;; https://www.owasp.org/index.php/Session_Management_Cheat_Sheet

;; Special variables
(defparameter *session-prefix* "hts:"
  "Session prefix can be handy in some backends, for example when
  using redis you can lookup all sessions ids using the \"keys hts:*\"
  command")

(defparameter *session-max-time* #.(* 30 60)
  "Seconds duration after which session expires if not used, default
  is 30 minutes")
              
(defparameter *notify-session-forgery* nil
  "When set to T, you will get a condition thrown when library detects
  forged session")

(defparameter *use-user-agent-for-sessions* t
  "Whether the 'User-Agent' header should be encoded into the session
string.  If this value is true, a session will cease to be accessible
if the client sends a different 'User-Agent' header.")

(defparameter *use-remote-addr-for-sessions* nil
  "Whether the client's remote IP \(as returned by REAL-REMOTE-ADDR)
should be encoded into the session string.  If this value is true, a
session will cease to be accessible if the client's remote IP changes.

This might for example be an issue if the client uses a proxy server
which doesn't send correct 'X_FORWARDED_FOR' headers.")

(defclass session ()
  ((session-id :initform nil ;(ht::next-session-id (request-acceptor *request*))
               :accessor session-id
               :type integer
               :documentation "The unique ID of the session.")
;;    (session-string :reader session-string
;;                    :documentation "The session string encodes enough
;; data to safely retrieve this session.  It is sent to the browser as a
;; cookie value or as a GET parameter.")
   (user-agent :initform nil ; (user-agent *request*)
               :accessor session-user-agent
               :documentation "The incoming 'User-Agent' header that
was sent when this session was created.")
   (remote-addr :initform nil ;(real-remote-addr *request*)
                :accessor session-remote-addr
                :documentation "The remote IP address of the client
when this session was started as returned by REAL-REMOTE-ADDR.")
   (session-start :initform (get-universal-time)
                  :reader session-start
                  :documentation "The time this session was started.")
   (last-click :initform (get-universal-time)
               :reader session-last-click
               :documentation "The last time this session was used.")
   (session-data :initarg :session-data
                 :initform nil
                 :reader session-data
                 :documentation "Data associated with this session -
see SESSION-VALUE.")
   (max-time :initarg :max-time
             :accessor session-max-time
             :type fixnum
             :documentation "The time \(in seconds) after which this
session expires if it's not used."))
   ;; (prototype :initform (make-instance 'cl-json::prototype
   ;;                                     :lisp-class "session"
   ;;                                     :lisp-package "hunchentoot-sessions")))
  (:default-initargs
   :max-time *session-max-time*
   :session-data (make-hash-table :test 'equal)))

(let ((n 32)
      (prng (ironclad:make-prng :fortuna)))
  (defun random-session-id ()
    "Generate random session id"
    (ironclad:byte-array-to-hex-string (ironclad:random-data n prng))))

(defmethod initialize-instance :after ((session session) &rest init-args)
  "Set session slots after it has been initialized."
  (declare (ignore init-args))

  (when (not (session-id session))
      (setf (session-id session)
            (concatenate 'string
                         *session-prefix*
                         (random-session-id))))
  
  (when (boundp 'ht:*request*)
     ;  (ht::next-session-id (request-acceptor *request*))))
    
    (when (not (session-user-agent session))
      (setf (session-user-agent session) (ht:user-agent ht:*request*)))
    (when (not (session-remote-addr session))
      (setf (session-remote-addr session) (ht:real-remote-addr ht:*request*)))))

  ;; (when (zerop (hash-table-count (session-data session)))
  ;;   (setf (gethash 'prototype (session-data session)) 'hash-table)))

(defun start-session ()
  "Returns the current SESSION object. If there is no current session,
creates one and updates the corresponding data structures. In this
case the function will also send a session cookie to the browser."
  (let ((session (ht:session ht:*request*)))
    (when session
      (return-from start-session session))
    
    (setf session (make-instance 'session)
          (ht:session ht:*request*) session)

    (save-session session)
    
    (ht:set-cookie (session-cookie-name ht:*acceptor*)
                   :value (session-cookie-value session)
                   :path "/")
    
    (ht:session-created ht:*acceptor* session)
    (setq ht:*session* session)
    
    session))

(defun session-value (symbol &optional (session ht:*session*))
  (gethash symbol (session-data session)))

(defsetf session-value (symbol &optional session)
    (new-value)
  ""
  (with-gensyms (%session)
    `(let ((,%session (or ,session (start-session))))
       (setf (gethash ,symbol (session-data ,%session)) ,new-value)
       (save-session ,%session))))

(defmethod (setf session-data) (data (session session))
  ""
  (setf (slot-value session 'session-data) data)
  (save-session session))
                ;(session-id session)
                ;(serialize-session session)))

(defmethod save-session ((session session))
  "Save session"
  (backend-set (lookup-backend)
               (session-id session)
               (serialize-session session)))

(defun load-session (id)
  "Load session from backend"
  (when-let ((session-json (backend-get (lookup-backend) id)))
    (let ((session (deserialize-session session-json)))
      (when (and session
                 (session-too-old-p session))
        (delete-session session)
        (setq session nil))
      session)))

(defun delete-session (session)
  (backend-delete (lookup-backend) (session-id session)))

(defun serialize-session (session)
  (cl-json:encode-json-to-string session))

(defun deserialize-session (data)
  "Deserialize session, data must be a json string. Doing it manually
for now"
  (let ((obj (json:decode-json-from-string data))
        (session (make-instance 'session)))
    (mapcar #'(lambda (pair)
                (setf (slot-value session (find-symbol (symbol-name (car pair)) 'hunchentoot-sessions))
                      (cdr pair)))
            obj)
    (when (listp (session-data session))
      (setf (slot-value session 'session-data)
            (alexandria:alist-hash-table (session-data session))))
    session))    
  ;; (let ((cl-json:*json-symbols-package* 'cl-user))
  ;;   (json:with-decoder-simple-clos-semantics
  ;;     (cl-json:decode-json-from-string data))))

;; backend

(defgeneric backend-for-acceptor (acceptor)
  (:documentation ""))

(defun lookup-backend ()
  "Lookup what backend to use"
  (or (and (boundp '*backend*) *backend*)
      (backend-for-acceptor ht:*acceptor*)))

;; cookies

(defmethod session-cookie-value ((session session))
  (and session
       (format nil
               "~A"
               (session-id session))))

(defmethod session-cookie-name ((acceptor ht:acceptor))
  "hts")

(define-condition session-possibly-forged (error)
  ((text :initarg :text :reader text)
   (session :initarg :session :reader session)))

(defparameter *sess* nil)

(defmethod session-verify ((request ht:request))
  (let ((session-identifier (or (when-let (session-cookie (ht:cookie-in (session-cookie-name ht:*acceptor*) request))
                                  (ht:url-decode session-cookie))
                                (ht:get-parameter (session-cookie-name ht:*acceptor*) request))))
    (unless (and session-identifier
                 (stringp session-identifier)
                 (plusp (length session-identifier)))
      (return-from session-verify nil))
    ;; (destructuring-bind (id session-string)
    ;;     (split ":" session-identifier :limit 2)
    (let ((session (load-session session-identifier)))
      (cond ((and session
                  (check-session-fingerprint session request))
             (setf (slot-value session 'last-click) (get-universal-time))
             session)
        
            (session
             ;; the session ID pointed to an existing session, but the
             ;; session string did not match the expected session string
             ;; remove the session to make sure that it can't be used
             ;; again; the original legitimate user will be required to
             ;; log in again
             (when *notify-session-forgery*
               (error 'session-possibly-forged
                      :text "Session possibly forged"
                      :session session))
             (delete-session session)
             nil)
            
            (t
             ;; no session was found under the ID given, presumably
             ;; because it has expired.
             nil)))))

(defun check-session-fingerprint (session request)
  (when *use-user-agent-for-sessions*
    (when (not (eq (ht:user-agent request) (session-user-agent session)))
      nil))
  (when *use-remote-addr-for-sessions*
    (when (not (eq (ht:real-remote-addr request) (session-remote-addr session)))
      nil))
  t)

(defun session-too-old-p (session)
  "Returns true if the SESSION object SESSION has not been active in
the last \(SESSION-MAX-TIME SESSION) seconds."
  (< (+ (session-last-click session) (session-max-time session))
     (get-universal-time)))

(defun create-random-string (&optional (n 32))
  (ironclad:byte-array-to-hex-string (ironclad:random-data n)))
