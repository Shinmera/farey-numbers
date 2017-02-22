#|
 This file is a part of Farey-Numbers
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(defpackage #:farey-numbers
  (:nicknames #:farey #:org.shirakumo.farey-numbers)
  (:use #:cl)
  (:export
   #:generate-farey-grid
   #:*farey-grid*
   #:fareyref
   #:farey
   #:make-farey
   #:snap-to-farey-grid
   #:ensure-farey
   #:multiplier
   #:grid-index
   #:fraction
   #:farey-number
   #:combine
   #:f+
   #:f*
   #:f-
   #:f/
   #:fshift))
