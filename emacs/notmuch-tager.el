;; notmuch-tager.el --- Library to show labels as links
;;
;; Copyright © Damien Cassou
;;
;; This file is part of Notmuch.
;;
;; Notmuch is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Notmuch is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Authors: Damien Cassou <damien.cassou@gmail.com>
;;; Commentary:
;;
;;; Code:
;;

(require 'button)
(require 'header-button)

(defun notmuch-tager-separate-elems (list sep)
  "Return a list with all elements of LIST separated by SEP."
  (let ((first t)
        (res nil))
    (dolist (elt (reverse list) res)
      (unless first
        (push sep res))
      (setq first nil)
      (push elt res))))

(defun notmuch-tager-goto-target (target)
  "Show a `notmuch-search' buffer for the TARGET tag."
  (notmuch-search (concat "tag:" target)))

(defun notmuch-tager-button-action (button)
  "Open `notmuch-search' for the tag referenced by BUTTON."
  (let ((tag (header-button-get button :notmuch-tager-tag)))
    (notmuch-tager-goto-target tag)))

(define-button-type 'notmuch-tager-button-type
  :supertype 'header
  :action    'notmuch-tager-button-action
  :follow-link t)

(defun notmuch-tager-make-link (target)
  "Return a property list that presents a link to TARGET.

TARGET is a notmuch tag."
  (header-button-format
   target
   :type 'notmuch-tager-button-type
   :notmuch-tager-tag target
   :help-echo (format "%s: Search other messages like this" target)))

(defun notmuch-tager-format-tags (tags)
  "Return a format list for TAGS suitable for use in header line.
See Info node `(elisp)Mode Line Format' for more information."
  (mapcar 'notmuch-tager-make-link tags))

(defun notmuch-tager-present-tags (tags)
  "Return a property list which nicely presents all TAGS."
  (list
   " ("
   (notmuch-tager-separate-elems (notmuch-tager-format-tags tags) ", ")
   ")"))

(provide 'notmuch-tager)
;;; notmuch-tager.el ends here
