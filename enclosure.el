;;; enclosure.el --- Adds helpers for working with enclosing pairs                      -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jamie Cunliffe <jamie.cunliffe@outlook.com>
;; Author: Jamie Cunliffe <jamie.cunliffe@outlook.com>
;; Package-Requires: ((dash "2.14.1"))
;; URL: http://github.com/JamieCunliffe/enclosure
;; Version: 0.1

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
(require 'dash)

(defvar enclosure-chars
  '((:beginning "(" :end ")")
    (:beginning "{" :end "}")
    (:beginning "[" :end "]")
    (:beginning "<" :end ">")))

(defun enclosure--get-add-string()
  "Prompts the user for the sequence to add."
  (read-string "Add sequence: "))

(defun enclosure--get-change-string()
  "Prompts the user for the sequence to change."
  (let* ((change (read-string "Replace sequence: "))
         (add (read-string (format "Replace sequence %s with: " change))))
    (cons change add)))

(defun enclosure--get-delete-string()
  "Prompts the user for the sequence to delete."
  (read-string "Delete sequence: "))

(defun enclosure--find-str-pos-same-pair(sym)
  "Find the start and end for sym.
SYM the symbol to find the previous and next of."
  (let* ((beg)
         (end))
    (search-backward sym)
    (setq beg (point))
    (forward-char)
    (search-forward sym)
    (setq end (point))
    (list beg end)))

(defun enclosure--find-str-pos-diff-pair(beg-sym end-sym)
  "Find the start and end positions for beg-sym and and end-sym.
BEG-SYM The opening symbol to look for
END-SYM The closing symbol to look for"
  (let* ((ctr 0)
         (start (point))
         (beg 0)
         (end 0))
    (while (not (and (looking-at beg-sym) (= ctr 0)))
      (if (looking-at end-sym)
          (setq ctr (+ 1 ctr)))
      (if (looking-at beg-sym)
          (setq ctr (- ctr 1)))
      (backward-char 1))
    (setq beg (point))
    (goto-char start)
    (while (not (and (looking-at end-sym) (= ctr 0)))
      (if (looking-at beg-sym)
          (setq ctr (+ 1 ctr)))
      (if (looking-at end-sym)
          (setq ctr (- ctr 1)))
      (forward-char 1))
    (setq end (+ (point) (length end-sym)))
    (list beg end)))


(defun enclosure--find-str-pos-for-pair(beg-sym end-sym)
  "Find the start and end of the pair specified.
BEG-SYM The opening symbol.
END-SYM The closing symbol."
  (if (string= beg-sym end-sym)
      (enclosure--find-str-pos-same-pair beg-sym)
    (enclosure--find-str-pos-diff-pair beg-sym end-sym)))

(defun enclosure--get-pair(input-str)
  "Find the pair in the delimiter pair in the list.
INPUT-STR the pair type to find"
  (-first (lambda(item)
            (or
             (string= (plist-get item :beginning) input-str)
             (string= (plist-get item :end) input-str)))
          enclosure-chars))

(defun enclosure--add-chars(beg end &optional input-char)
  "Prompts for char to insert and insert at beg and end.
BEG Start position.
END End position."
  (let* ((input-str (if input-char input-char (enclosure--get-add-string)))
         (pair (enclosure--get-pair input-str))
         (beg-symbol (if pair (plist-get pair :beginning) input-str))
         (end-symbol (if pair (plist-get pair :end) input-str)))
    (save-excursion
      (goto-char end)
      (insert end-symbol)
      (goto-char beg)
      (insert beg-symbol))))

;;;###autoload
(defun enclosure-delete()
  "Delete."
  (interactive)
  (save-excursion
    (let* ((input-str (enclosure--get-delete-string))
           (pair (enclosure--get-pair input-str))
           (beg-symbol (if pair (plist-get pair :beginning) input-str))
           (end-symbol (if pair (plist-get pair :end) input-str))
           (beg-len (length beg-symbol))
           (end-len (length end-symbol))
           (position (enclosure--find-str-pos-for-pair beg-symbol end-symbol))
           (beg (car position))
           (end (car (cdr position))))
      (goto-char (- end end-len))
      (delete-char end-len)
      (goto-char beg)
      (delete-char beg-len))))

;;;###autoload
(defun enclosure-change()
  "Change."
  (interactive)
  (save-excursion
    (let* ((change-pair (enclosure--get-change-string))
           (delete-char (car change-pair))
           (insert-char (cdr change-pair))
           (pair (enclosure--get-pair delete-char))
           (beg-symbol (if pair (plist-get pair :beginning) delete-char))
           (end-symbol (if pair (plist-get pair :end) delete-char))
           (beg-len (length beg-symbol))
           (end-len (length end-symbol))
           (position (enclosure--find-str-pos-for-pair beg-symbol end-symbol))
           (beg (car position))
           (end (car (cdr position))))
      (goto-char (- end end-len))
      (delete-char end-len)
      (goto-char beg)
      (delete-char beg-len)
      (enclosure--add-chars beg (- end 1 beg-len (- end-len beg-len)) insert-char))))

;;;###autoload
(defun enclosure-region ()
  "Surrounds the region with the prompted string."
  (interactive)
  (enclosure--add-chars (region-beginning) (region-end)))

;;;###autoload
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
;;; enclosure.el ends here
