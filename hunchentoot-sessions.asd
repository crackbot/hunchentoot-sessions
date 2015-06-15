
(defsystem :hunchentoot-sessions
  :description "Distributed sessions for hunchentoot, supports Redis and Memcached backends."
  :version "0.0.1"
  :author "Crackbot <thecrackbot@gmail.com>"
  :maintainer "Crackbot <thecrackbot@gmail.com>"
  :license "The MIT License (MIT)"
  :components ((:static-file "hunchentoot-sessions.asd")
               (:module "src"
                        :components ((:file "package")
                                     (:module "backend"
                                              :components ((:file "base")
                                                           (:file "redis")
                                                           (:file "memcached")))
                                     (:file "main"))))
  :depends-on (:cl-json :cl-redis :cl-ppcre :hunchentoot :ironclad :alexandria))
