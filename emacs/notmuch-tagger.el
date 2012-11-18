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

(defun notmuch-tagger-present-tags-header-line (tags)
  "Return a property list to present TAGS in emacs header-line."
  (list
   "("
   (notmuch-tagger-separate-elems tags " ")
   ")"))


(provide 'notmuch-tagger)
;;; notmuch-tagger.el ends here
