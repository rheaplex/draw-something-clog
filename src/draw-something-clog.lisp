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

(defparameter +width+ 7680)
(defparameter +height+ 4320)
(defparameter +savedir+ "/var/www/html/drawings")

(defun flop-y (y)
  "Invert the y-co-ordinate."
  (- +height+ y))

(defun draw-poly-points (cx poly)
  "Draw the polygon as a path in cx."
  (begin-path cx)
  (let ((points (ds::points poly)))
    (move-to cx
             (round (ds::x (aref points 0)))
             (flop-y (round (ds::y (aref points 0)))))
    (loop for i from 1 below (length points)
          do (line-to cx
                      (round (ds::x (aref points i)))
                      (flop-y (round (ds::y (aref points i))))))))

(defun fill-poly (cx poly fill)
  "Draw and fillthe polygon as a path in cx."
  (setf (stroke-style cx) :none)
  (setf (fill-style cx) fill)
  (draw-poly-points cx poly)
  (path-fill cx))

(defun stroke-poly (cx poly stroke-style stroke-width)
  "Draw and stroke the polygon as a path in cx."
  (setf (fill-style cx) :none)
  (setf (line-width cx) stroke-width)
  (setf (stroke-style cx) stroke-style)
  (draw-poly-points cx poly)
  (path-stroke cx))

(defun save-dir-path (savedir time)
  "Use subdirectories for each day of drawings to hackily paginate."
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time time)
    (declare (ignorable seconds minutes hours))
    (let ((subdir (format nil  "~2,,,'0@A-~2,,,'0@A-~2,,,'0@A"
                          year month date)))
      (make-pathname :directory
                     (list :relative savedir subdir)))))

(defun save-file-name (time)
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time time)
    (declare (ignorable date month year))
    (format nil  "~2,,,'0@A-~2,,,'0@A-~2,,,'0@A"
                            hours minutes seconds)))

(defun draw (cx nodelay)
  (let* ((now (get-universal-time))
         (savesubdir (save-dir-path +savedir+ now))
         (savefilename (save-file-name now))
         (drawing (ds:draw-something :randseed now
                                     :savedir savesubdir
                                     :filename savefilename)))
    ;; Draw the background
    (setf (fill-style cx) (ds::colour-to-rgb-hex (ds::ground drawing)))
    (fill-rect cx 0 0 +width+ +height+)
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
                          (flop-y (ds::y (ds::from line))))
                 (line-to cx
                          (ds::x (ds::to line))
                          (flop-y (ds::y (ds::to line))))
                 (path-stroke cx)
                 (unless nodelay
                   (sleep 0.01))))
      (unless nodelay
        (sleep 0.5))
      ;; Draw the pen outline to show path construction
      (ds::do-poly-lines ((ds::outline form) line)
        (setf (stroke-style cx) (ds::colour-to-rgb-hex (ds::fill-colour form)))
        (begin-path cx)
        (move-to cx
                 (round (ds::x (ds::from line)))
                 (round (flop-y (ds::y (ds::from line)))))
        (line-to cx
                 (round (ds::x (ds::to line)))
                 (round (flop-y (ds::y (ds::to line)))))
        (path-stroke cx)
        (unless nodelay
          (sleep 0.01)))
      (unless nodelay
        (sleep 0.5))
      ;; Draw the polygon fill
      (fill-poly cx
                 (ds::outline form)
                 (ds::colour-to-rgb-hex(ds::fill-colour form)))
      (when (ds::stroke-colour form)
        (stroke-poly cx
                     (ds::outline form)
                     (ds::colour-to-rgb-hex (ds::stroke-colour form))
                     (ds::stroke-width form)))
      (unless nodelay
        (sleep 0.25)))))

(defun on-new-window (body)
  (setf (title (html-document body)) "draw-something")
  ;; Centre the page content (the canvas)
  (set-styles body
              '(("border" 0)
                ("margin" 0)
                ("padding" 0)
                ("cursor" "none")
                ("height" "100vh")
                ("display" "flex")
                ("align-items" "center")
                ("justify-content" "center")))
  ;;FIXME: switch to form-get-data, which will return ((nodelay))
  (let* ((nodelay   (not (equal (search "nodelay" (url (location body))) nil)))
         (canvas    (create-canvas body :width +width+ :height +height+))
         (cx        (create-context2d canvas))
         (interrupt nil))
    ;; Scale the canvas to the page
    (set-styles canvas
                '(("width" "100vw"
                   "height" "56.25vw")))
    (setf (font-style cx) "bold 20vh sans-serif")
    (fill-text cx
               "draw-something is connected and initalizing, please wait 2 minutes..."
               10
               (/ +height+ 2))
    (set-on-key-press body
                      (lambda (obj data)
                        (declare (ignore obj data))
                        (setf interrupt t)))
    ;; Loop forever, making a new drawing once per minute or so
    (loop
      do (draw cx nodelay)
         (loop for i from 0 below 60
               if (equal interrupt t)
                 do (return)
               do (sleep 1))
         (setf interrupt nil))))

(defun start ()
  "Start the server in test mode."
  (setf +savedir+ "/tmp")
  (initialize 'on-new-window)
  (open-browser))
