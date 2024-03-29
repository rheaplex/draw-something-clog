;; package.lisp - The main package for draw-something-clog
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

(in-package :cl-user)

(defpackage "DRAW-SOMETHING-CLOG"
  (:use :cl :clog :draw-something)
  (:export
   #:start))
