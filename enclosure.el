;;; package --- enclosure.el                      -*- lexical-binding: t; -*-
;;; Commentary:
; enclosure.el
;; Author: Jamie Cunliffe <jamie.cunliffe@outlook.com>
;; URL: http://github.com/JamieCunliffe/enclosure

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar enclosure--chars
  '((:beginning "(" :end ")")
    (:beginning "{" :end "}")
    (:beginning "[" :end "]")
    (:beginning "<" :end ">")))


(defun enclosure--get-pair(input-str)
  "Find the pair in the delimiter pair in the list.
INPUT-STR the pair type to find"
  (-first (lambda(item)
            (or
             (string= (plist-get item :beginning) input-str)
             (string= (plist-get item :end) input-str)))
          enclosure--chars))

(defun enclosure--add-chars(beg end)
  "Prompts for char to insert and insert at beg and end.
BEG Start position.
END End position."
  (let* ((input-str (read-string "Char: "))
         (pair (enclosure--get-pair input-str))
         (beg-symbol (if pair (plist-get pair :beginning) input-str))
         (end-symbol (if pair (plist-get pair :end) input-str)))
    (save-excursion
      (goto-char end)
      (insert end-symbol)
      (goto-char beg)
      (insert beg-symbol))))

(defun enclosure-region ()
  "Surrounds the region with the prompted string."
  (interactive)
  (enclosure--add-chars (region-beginning) (region-end)))

(defun enclosure-thing-at-point()
  "Surrounds the 'thing-at-point' with the prompted string."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (beg (car bounds))
         (end (cdr bounds)))
    (if bounds
        (enclosure--add-chars beg end)
      (error "Nothing at point"))))

(provide 'enclosure)
;;; enclosure.el ends here- 
