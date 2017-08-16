#|
 This file is a part of Farey-Numbers
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem farey-numbers
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library to operate with numbers on the Farey sequence."
  :homepage "https://github.com/Shinmera/farey-numbers"
  :serial T
  :components ((:file "package")
               (:file "farey")
               (:file "documentation"))
  :depends-on (:documentation-utils))
