;; notmuch-tagger.el --- Library to show labels as links
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

(or (require 'header-button nil t)
    (let ((load-path
           (cons (expand-file-name
                  "fallback-libs"
                  (file-name-directory (or load-file-name buffer-file-name)))
                 load-path)))
      (require 'header-button)))

(defun notmuch-tagger-separate-elems (list sep)
  "Return a list with all elements of LIST separated by SEP."
  (let ((first t)
        (res nil))
    (dolist (elt (reverse list) res)
      (unless first
        (push sep res))
      (setq first nil)
      (push elt res))))

(defun notmuch-tagger-goto-target (target)
  "Show a `notmuch-search' buffer for the TARGET tag."
  (notmuch-search (concat "tag:" target)))

(defun notmuch-tagger-headerline-button-action (button)
  "Open `notmuch-search' for the tag referenced by BUTTON."
  (let ((tag (header-button-get button 'notmuch-tagger-tag)))
    (notmuch-tagger-goto-target tag)))

(defun notmuch-tagger-body-button-action (button)
  "Open `notmuch-search' for the tag referenced by BUTTON."
  (let ((tag (button-get button 'notmuch-tagger-tag)))
    (notmuch-tagger-goto-target tag)))

(define-button-type 'notmuch-tagger-headerline-button-type
  'supertype 'header
  'action    #'notmuch-tagger-headerline-button-action
  'follow-link t)

(define-button-type 'notmuch-tagger-body-button-type
  'action    #'notmuch-tagger-body-button-action
  'follow-link t)

(defun notmuch-tagger-make-headerline-link (target)
  "Return a property list that presents a link to TARGET.

TARGET is a notmuch tag.
The returned property list will only work in the header-line."
  (header-button-format
   target
   :type 'notmuch-tagger-headerline-button-type
   'notmuch-tagger-tag target
   'help-echo (format "%s: Search other messages like this" target)))

(defun notmuch-tagger-make-body-link (target)
  "Return a property list that presents a link to TARGET.

TARGET is a notmuch tag.
The returned property list will work everywhere except in the
header-line."
  (let ((button (copy-sequence target)))
    (make-text-button
     button nil
     'type 'notmuch-tagger-body-button-type
     'notmuch-tagger-tag target
     'help-echo (format "%s: Search other messages like this" target))
    button))

(defun notmuch-tagger-make-link (target headerline)
"Return a property list that presents a link to TARGET.

TARGET is a notmuch tag.

If HEADERLINE is non-nil the returned list will be ready for
inclusion in the buffer's header-line (i.e., will use the
`header-button' library if available). Otherwise it returns a
property list ready for inclusion in a buffer (through
`format-mode-line')."
  (if headerline
      (notmuch-tagger-make-headerline-link target)
    (notmuch-tagger-make-body-link target)))

(defun notmuch-tagger-format-tags (tags &optional headerline)
  "Return a format list for TAGS suitable for use in header line.
See Info node `(elisp)Mode Line Format' for more information.

If HEADERLINE is non-nil the returned list will be ready for
inclusion in the buffer's header-line (i.e., will use the
`header-button' library if available). Otherwise it returns a
property list ready for inclusion in a buffer (through
`format-mode-line')."
  (mapcar
   (lambda (tag) (notmuch-tagger-make-link tag headerline))
   tags))

(defun notmuch-tagger-present-tags (tags &optional headerline)
  "Return a property list which nicely presents all TAGS.

If HEADERLINE is non-nil the returned list will be ready for
inclusion in the buffer's header-line (i.e., will use the
`header-button' library if available). Otherwise it returns a
property list ready for inclusion in a buffer (through
`format-mode-line')."
  (list
   "("
   (notmuch-tagger-separate-elems (notmuch-tagger-format-tags tags headerline) " ")
   ")"))

(provide 'notmuch-tagger)
;;; notmuch-tagger.el ends here
