
(in-package :hunchentoot-sessions)

(defclass memcached-backend (backend)
  ((connection :reader mem-conn))
  (:default-initargs
   :host "localhost"
   :port 11211))

;; (defun create-memcached-backend (&key (host "localhost") (port 11211))
;;   (let ((mem (make-instance 'memcached-backend :host host :port port)))
;;     (setf (slot-value mem 'connection) (cl-memcached:make-memcache :ip host :port port)
;;           *backend* mem)))

;; (defmethod backend-set ((backend memcached-backend) id data)
;;   (cl-memcached:mc-set id data :memcache (mem-conn backend)))

;; (defmethod backend-get ((backend memcached-backend) id)
;;   (cl-memcached:mc-get id :memcache (mem-conn backend)))

;; (defmethod backend-delete ((backend memcached-backend) id)
;;   (cl-memcached:mc-del id :memcache (mem-conn backend)))
