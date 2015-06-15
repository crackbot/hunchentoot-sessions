
(in-package :hunchentoot-sessions)

(defparameter *backend* nil
  "Backend for sessions")

(defclass backend ()
  ((host :initarg :host
         :accessor session-backend-host
         :type string
         :documentation "Backend host")
   (port :initarg :port
         :accessor session-backend-port
         :type integer
         :documentation "Backend port")))
