;; notmuch-query.el --- provide an emacs api to query notmuch
;;
;; Copyright © David Bremner
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
;; Authors: David Bremner <david@tethera.net>

(require 'notmuch-lib)
(require 'json)

(defun notmuch-query-get-threads (search-terms)
  "Return a list of threads of messages matching SEARCH-TERMS.

A thread is a forest or list of trees. A tree is a two element
list where the first element is a message, and the second element
is a possibly empty forest of replies.
"
  (let  ((args '("show" "--format=json"))
	 (json-object-type 'plist)
	 (json-array-type 'list)
	 (json-false 'nil))
    (if notmuch-show-process-crypto
	(setq args (append args '("--decrypt"))))
    (setq args (append args search-terms))
    (with-temp-buffer
      (progn
	(apply 'call-process (append (list notmuch-command nil (list t nil) nil) args))
	(goto-char (point-min))
	(json-read)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mapping functions across collections of messages.

(defun notmuch-query-map-aux  (mapper function seq)
  "private function to do the actual mapping and flattening"
  (apply 'append
	 (mapcar
	   (lambda (tree)
	     (funcall mapper function tree))
	   seq)))

(defun notmuch-query-map-threads (fn threads)
  "apply FN to every thread in  THREADS. Flatten results to a list.

See the function notmuch-query-get-threads for more information."
  (notmuch-query-map-aux 'notmuch-query-map-forest fn threads))

(defun notmuch-query-map-forest (fn forest)
  "apply function to every message in a forest. Flatten results to a list.

See the function notmuch-query-get-threads for more information.
"
  (notmuch-query-map-aux 'notmuch-query-map-tree fn forest))

(defun notmuch-query-map-tree (fn tree)
  "Apply function FN to every message in TREE. Flatten results to a list

See the function notmuch-query-get-threads for more information."
  (cons (funcall fn (car tree)) (notmuch-query-map-forest fn (cadr tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predefined queries

(defun notmuch-query-get-message-ids (&rest search-terms)
  "Return a list of message-ids of messages that match SEARCH-TERMS"
  (notmuch-query-map-threads
   (lambda (msg) (plist-get msg :id))
   (notmuch-query-get-threads search-terms)))

(defun notmuch-query-thread-tags-from-id (thread-id)
  "Return the tags of thread whose id is THREAD-ID.
The thread tags are the union of the tags of emails in the
thread."
  (let ((tag-lists
	 (notmuch-query-map-forest
	  (lambda (msg) (plist-get msg :tags))
	  (car (notmuch-query-get-threads
		(list (concat "thread:" thread-id)))))))
    (case (length tag-lists)
      (0 nil)
      (1 (car tag-lists))
      (otherwise (reduce (lambda (l1 l2)
			   (union l1 l2 :test 'string=))
			 tag-lists)))))

(provide 'notmuch-query)
