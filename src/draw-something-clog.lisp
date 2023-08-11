;; draw-something-clog.lisp - A web-based draw-something.
;; Copyright (C) 2023 Myers Studio Ltd.
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

(in-package :draw-something-clog)

(defun flop-y (y height)
  "Invert the y-co-ordinate."
  (- height y))

(defun draw-poly-points (cx canvas poly)
  "Draw the polygon as a path in cx."
  (begin-path cx)
  (let ((height (height canvas))
        (points (ds::points poly)))
    (move-to cx
             (round (ds::x (aref points 0)))
             (flop-y (round (ds::y (aref points 0))) height))
    (loop for i from 1 below (length points)
          do (line-to cx
                      (round (ds::x (aref points i)))
                      (flop-y (round (ds::y (aref points i))) height)))))

(defun fill-poly (cx canvas poly fill)
  "Draw and fillthe polygon as a path in cx."
  (setf (stroke-style cx) :none)
  (setf (fill-style cx) fill)
  (draw-poly-points cx canvas poly)
  (path-fill cx))

(defun stroke-poly (cx poly canvas stroke-style stroke-width)
  "Draw and stroke the polygon as a path in cx."
  (setf (fill-style cx) :none)
  (setf (line-width cx) stroke-width)
  (setf (stroke-style cx) stroke-style)
  (draw-poly-points cx canvas poly)
  (path-stroke cx))

(defun on-new-window (body)
  (setf (title (html-document body)) "draw-something")
  (set-styles body
              '(("border" 0)
                ("margin" 0)
                ("padding" 0)
                ("cursor" "none")
                ("height" "100vh")
                ("display" "flex")
                ("align-items" "center")
                ("justify-content" "center")))
  (let* ((canvas  (create-canvas body :width 1280 :height 768))
         (cx      (create-context2d canvas))
         (drawing (ds:draw-something)))
    ;; Draw the background
    (setf (fill-style cx) (ds::colour-to-rgb-hex (ds::ground drawing)))
    (fill-rect cx 0 0 (width canvas) (height canvas))
    (setf (line-cap cx) :round)
    (setf (line-join cx) :round)
    ;; Draw each form 
    (ds::do-drawing-forms (drawing form)
      ;; Draw the form skeleton
      (loop for skeleton across (ds::skeleton form)
            do (ds::do-poly-lines (skeleton line)
                 (setf (stroke-style cx)
                       (ds::colour-to-rgb-hex (ds::fill-colour form)))
                 (begin-path cx)
                 (move-to cx
                          (ds::x (ds::from line))
                          (flop-y (ds::y (ds::from line)) (height canvas)))
                 (line-to cx
                          (ds::x (ds::to line))
                          (flop-y (ds::y (ds::to line)) (height canvas)))
                 (path-stroke cx)
                 (sleep 0.01)))
      (sleep 0.5)
      ;; Draw the pen outline to show path construction
      (ds::do-poly-lines ((ds::outline form) line)
        (setf (stroke-style cx) (ds::colour-to-rgb-hex (ds::fill-colour form)))
        (begin-path cx)
        (move-to cx
                 (round (ds::x (ds::from line)))
                 (round (flop-y (ds::y (ds::from line)) (height canvas))))
        (line-to cx
                 (round (ds::x (ds::to line)))
                 (round (flop-y (ds::y (ds::to line)) (height canvas))))
        (path-stroke cx)
        (sleep 0.01))
      (sleep 0.5)
      ;; Draw the polygon fill
      (fill-poly cx
                 canvas
                 (ds::outline form)
                 (ds::colour-to-rgb-hex(ds::fill-colour form)))
      (when (ds::stroke-colour form)
        (stroke-poly cx
                     canvas
                     (ds::outline form)
                     (ds::colour-to-rgb-hex (ds::stroke-colour form))
                     (ds::stroke-width form)))
      (sleep 0.25)))
  ;; Pause for a minute then reload the page
  (sleep 60)
  (reload (location body)))

(defun start ()
  "Start the server."
  (initialize 'on-new-window)
  (open-browser))
