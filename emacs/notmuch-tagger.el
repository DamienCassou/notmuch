;; notmuch-tagger.el --- Library to improve the way tags are displayed
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
(defun notmuch-tagger-separate-elems (list sep)
  "Return a list with all elements of LIST separated by SEP."
  (let ((first t)
        (res nil))
    (dolist (elt (reverse list) res)
      (unless first
        (push sep res))
      (setq first nil)
      (push elt res))))

(defun notmuch-tagger-header-button-present-p ()
  "Check if `header-button' can be loaded or is already loaded.

`header-button' is a third-party library which facilitates the
creation of links in emacs header-line. This function tries to
`require' `header-button' and returns nil if and only if this
fails."
  (require 'header-button nil t))

(defun notmuch-tagger-goto-target (tag)
  "Show a `notmuch-search' buffer for the TAG."
  (notmuch-search (concat "tag:" tag)))

(defun notmuch-tagger-header-button-action (button)
  "Open `notmuch-search' for the tag referenced by BUTTON.
This function depends on the presence of the `header-button'
library. Please call `notmuch-tagger-header-button-present-p' to
test if the library is present before calling this function."
  (let ((tag (header-button-get button 'notmuch-tagger-tag)))
    (notmuch-tagger-goto-target tag)))

(defun notmuch-tagger-body-button-action (button)
  "Open `notmuch-search' for the tag referenced by BUTTON."
  (let ((tag (button-get button 'notmuch-tagger-tag)))
    (notmuch-tagger-goto-target tag)))

(eval-after-load "header-button"
  '(define-button-type 'notmuch-tagger-header-button-type
     'supertype 'header
     'action    #'notmuch-tagger-header-button-action
     'follow-link t))

(define-button-type 'notmuch-tagger-body-button-type
  'action    #'notmuch-tagger-body-button-action
  'follow-link t)

(defun notmuch-tagger-really-make-header-link (tag)
   "Return a property list that presents a link to TAG.

The returned property list will only work in the header-line.
Additionally, this function depends on the presence of the
`header-button' library. Please call
`notmuch-tagger-header-button-present-p' to test if library is
present before calling this function."
   (header-button-format
    tag
    :type 'notmuch-tagger-header-button-type
    'notmuch-tagger-tag tag
    'help-echo (format "%s: Search other messages like this" tag)))

(defun notmuch-tagger-make-header-link (tag)
  "Return a property list to present TAG as a link to search.

This only works if `header-button' is loaded. Simply returns tag
if not."
  (if (notmuch-tagger-header-button-present-p)
      (notmuch-tagger-really-make-header-link tag)
    tag))

(defun notmuch-tagger-make-body-link (tag)
  "Return a property list that presents a link to TAG.
The returned property list will work everywhere except in the
header-line. For a link that works on the header-line, prefer
`notmuch-tagger-make-header-link'."
  (let ((button (copy-sequence tag)))
    (make-text-button
     button nil
     'type 'notmuch-tagger-body-button-type
     'notmuch-tagger-tag tag
     'help-echo (format "%s: Search other messages like this" tag))
    button))

(defun notmuch-tagger-present-tags-header-line (tags)
  "Return a property list to present TAGS in emacs header-line."
  (list
   "("
   (notmuch-tagger-separate-elems
    (mapcar #'notmuch-tagger-make-header-link tags)
            " ")
   ")"))

(defun notmuch-tagger-present-tags (tags)
  "Return a property list to present TAGS in emacs.
If tags the result of this function is to be used within the
header-line, prefer `notmuch-tagger-present-tags-header-line'
instead of this function."
  (list
   "("
   (notmuch-tagger-separate-elems
    (mapcar #'notmuch-tagger-make-body-link tags)
            " ")
   ")"))

(provide 'notmuch-tagger)
;;; notmuch-tagger.el ends here
