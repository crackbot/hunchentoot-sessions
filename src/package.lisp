
(defpackage :hunchentoot-sessions
  (:use :cl :redis :alexandria :hunchentoot :cl-ppcre :cl-json)
  (:nicknames :hts)
  (:export :*session-prefix*
           :*session-max-time*
           :*notify-session-forgery*
           :*use-user-agent-for-sessions*
           :*use-remote-addr-for-sessions*
           
           :session
           :start-session
           :delete-session
           :session-value
           :session-data
           :session-verify
           :session-possibly-forged
           
           :create-redis-backend)
  
  (:local-nicknames (ht hunchentoot)))
