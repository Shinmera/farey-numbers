(asdf:defsystem farey-numbers
  :version "1.0.0"
  :license "Artistic"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library to operate with numbers on the Farey sequence."
  :homepage "https://github.com/Shinmera/farey-numbers"
  :serial T
  :components ((:file "package")
               (:file "farey")
               (:file "documentation"))
  :depends-on (:documentation-utils))
