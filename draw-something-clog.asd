;; draw-something-clog.asd - The main package for draw-something
;; Copyright (C) 2023 Myers Studio, Ltd.
;;
;; This file is part of draw-something-clog.
;;
;; draw-something is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; draw-something is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :asdf-user)

(defsystem "draw-something-clog"
  :description "A web-based version of draw-something."
  :version "2023.1"
  :author "Rhea Myers"
  :licence "GNU General Public License v3.0 or later"
  :depends-on (#:clog #:draw-something)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "draw-something-clog")))
