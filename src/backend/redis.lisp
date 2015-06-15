
(in-package :hunchentoot-sessions)

(defclass redis-backend (backend)
  ()
  (:default-initargs
   :host "localhost"
   :port 6379))

(defun create-redis-backend (&key (host "localhost") (port 6379))
  (setf *backend*
        (make-instance 'redis-backend :host host :port port)))

(defmethod backend-set ((backend redis-backend) id data)
  (redis:with-recursive-connection ()
    (red:set id data)))

(defmethod backend-get ((backend redis-backend) id)
  (redis:with-recursive-connection ()
    (red:get id)))

(defmethod backend-delete ((backend redis-backend) id)
  (redis:with-recursive-connection ()
    (red:del id)))
