;;; org-glance.el --- org-mode traversing. Fast and convenient.

;; Copyright (C) 2018 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 0.1

;; Keywords: org-mode tools
;; Homepage: https://github.com/rails-to-cosmos/org-glance

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package allows you to manage bookmarks and travel around the
;; digital world with an org-mode power behind your shoulders.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defvar org-glance--scope-buffer-name "*org-glance-scope*")
(setq org-glance--cache (make-hash-table :test 'equal))

(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
   (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(defun org-glance (&rest args)
  "Use optional ARGS to customize your glancing blows:
- SCOPE :: org-file or SCOPE from org-map-entries (org.el)
- PROMPT :: completing read title (default: \"Glance: \")
- SEPARATOR :: completing read entry separator (default: \" → \")
- FILTER :: list or one filter of type lambda/symbol/string to specify entries in completing read.

  Possible default filters:
  - links :: keep entries with link in title
  - encrypted :: keep entries with :crypt: tag

  You can customize default filters by setting org-glance/default-filters variable.

- ACTION :: lambda to call on selected entry
  - if entry has an org-link in title, browse it
  - if entry has HANDLER property, read-eval it
- HANDLER :: property name to read-eval on select (default: \"HANDLER\")
- OUTLINE-IGNORE :: list of strings to ignore in outline-path

- INPLACE :: do not build scope file if specified

\(fn [:scope SCOPE] [:prompt PROMPT] [:separator SEPARATOR] [:filter FILTER] [:action ACTION] [:handler HANDLER])"
  (let* ((user-scopes (or (plist-get args :scope)          nil))
         (aggregated-scopes (org-glance--aggregate-scopes user-scopes))

         (user-filter (or (plist-get args :filter)       (lambda () t)))
         (filters (org-glance--filter-predicates user-filter))

         (outline-ignore (or (plist-get args :outline-ignore) nil))

         ;; user predicates
         (save-outline-visibility-p (or (plist-get args :save-outline-visibility-p) nil))
         (inplace-p                 (or (plist-get args :inplace) nil))

         (handler   (or (plist-get args :handler)        "HANDLER"))
         (prompt    (or (plist-get args :prompt)         "Glance: "))
         (separator (or (plist-get args :separator)      " → "))
         (action    (or (plist-get args :action)         (lambda nil (org-glance--handle-entry handler))))

         (entries (org-glance--entries
                   :scope aggregated-scopes
                   :separator separator
                   :outline-ignore outline-ignore
                   :filters filters
                   :inplace inplace-p))
         (-> (assert entries nil (format "Nothing to glance for in scopes %s" (prin1-to-string aggregated-scopes)))))

    (unwind-protect
        (org-glance/compl-map prompt entries action save-outline-visibility-p)
      (with-current-buffer (get-buffer-create org-glance--scope-buffer-name)
        (erase-buffer)
        (kill-buffer)))))

(defun org-glance--get-outline-path-and-marker-at-point (&rest args)
  "Return outline path of current `'org-mode`' entry.

Org node titles separated by SEPARATOR, titles specified in
OUTLINE-IGNORE will be ignored.

All FILTERS lambdas must be t."
  (let* ((separator           (or (plist-get args :separator)           org-glance-defaults--separator))
         (outline-ignore (or (plist-get args :outline-ignore) nil))
         (filters             (or (plist-get args :filters)             nil))

         (mark (point-marker))
         ;; (item (org-entry-get (point-marker) "ITEM"))

         (outline (cl-set-difference
                   (org-get-outline-path t)
                   outline-ignore
                   :test 'string=))

         ;; (item-outline (-snoc outline item))
         (title (mapconcat 'identity outline separator)))
    (when (cl-every (lambda (fp) (if fp (funcall fp) nil)) filters)
      (list title mark))))

(defun org-glance--entries (&rest args)
  "Return glance entries by SCOPE.

Specify SEPARATOR and OUTLINE-IGNORE to customize
outline-paths appearence.

When INPLACE flag specified, do not modify *org-glance-scope* buffer.

Add some FILTERS to filter unwanted entries."
  (let* ((scope               (or (plist-get args :scope)               nil))
         (-> (assert scope nil "You must specify :scope argument."))
         (-> (assert (listp scope) nil "Scope must be instance of list."))

         (separator           (or (plist-get args :separator)           org-glance-defaults--separator))
         (outline-ignore (or (plist-get args :outline-ignore) nil))
         (inplace-p           (or (plist-get args :inplace)             nil))
         (filters             (or (plist-get args :filters)             nil))

         ;; Possible beautify and optimization: switch to opened buffer instead of finding file
         ;; (live-buffers (remove nil (mapcar 'buffer-file-name (buffer-list))))
         (scope-type-getter (lambda (file-or-buffer)
                              (cond ((and (stringp file-or-buffer) (file-exists-p file-or-buffer)) 'file)
                                    ((and (bufferp file-or-buffer) (buffer-file-name file-or-buffer) (file-exists-p (buffer-file-name file-or-buffer))) 'file-buffer)
                                    ((bufferp file-or-buffer) 'buffer))))

         (scope-name-getter (lambda (file-or-buffer scope-type)
                              (case scope-type
                                ('file (expand-file-name file-or-buffer))
                                ('file-buffer (expand-file-name (buffer-file-name file-or-buffer)))
                                ('buffer (buffer-name file-or-buffer)))))

         (outliner (apply-partially
                    'org-glance--get-outline-path-and-marker-at-point
                    :separator separator
                    :outline-ignore outline-ignore
                    :filters filters))

         (implant (lambda (file-or-buffer scope-type)
                    ;; TODO: checkout if current entries already exist in org-glance-scope

                    (let ((entries (with-temp-buffer
                                     (org-mode)
                                     (case scope-type
                                       ('file (insert-file-contents file-or-buffer))
                                       ('file-buffer (insert-file-contents (buffer-file-name file-or-buffer)))
                                       ('buffer (insert-buffer-substring-no-properties file-or-buffer)))
                                     (mapcar 'car (org-map-entries outliner))))

                          (scope-name (funcall scope-name-getter file-or-buffer scope-type)))
                      (with-current-buffer (get-buffer-create org-glance--scope-buffer-name)
                        (org-mode)
                        (loop for entry in entries
                              initially (insert (format "* %s\n" scope-name))
                              when entry
                              do (insert (format " * %s\n" entry))
                              finally (insert "\n"))))))

         (visitor (lambda (file-or-buffer scope-type)
                    (save-window-excursion
                      (case scope-type
                        ('file (find-file file-or-buffer))
                        ('file-buffer (switch-to-buffer file-or-buffer))
                        ('buffer (switch-to-buffer file-or-buffer)))
                      (org-map-entries outliner))))

         (handler (if inplace-p visitor implant)))

    (loop for file-or-buffer in scope
          with scope-type do (setq scope-type (funcall scope-type-getter file-or-buffer))
          append (funcall handler file-or-buffer scope-type)

          ;; with scope-name do (setq scope-name (funcall scope-name-getter file-or-buffer scope-type))

          ;; with registered-scopes
          ;; do (setq registered-scopes
          ;;          (org-element-map
          ;;              (org-element-parse-buffer 'headline) 'headline
          ;;            (lambda (headline) (org-element-property :title headline))))
          ;; collect registered-scopes

          ;; when (and (not (member scope-name registered-scopes))
          ;;           entries)

          ;; do (cl-loop for entry in entries
          ;;             initially (insert (format "* %s\n" scope-name))
          ;;             when entry
          ;;             do (insert (format " * %s\n" entry))
          ;;             finally (insert "\n"))
          ))


  ;; (org-map-entries
  ;;  #'(lambda () (org-glance--get-outline-path-and-marker-at-point
  ;;           separator outline-ignore filter-predicates)))
  )

(defun org-glance--handle-entry (handler)
  "Try to handle current org-entry:
1. If there is an org-link, browse it.
2. If not, read-eval HANDLER property."
  (cond ((org-match-line (format "^.*%s.*$" org-bracket-link-regexp)) (org-glance/follow-org-link-at-point))
        ((org-entry-get nil handler) (let ((action (read (org-entry-get nil handler))))
                                       (cond ((symbolp action) (read (macroexpand (list 'org-sbe (symbol-name action)))))
                                             (t (eval action)))))))

(defun org-glance/compl-map (prompt entries action &optional save-outline-visibility-p)
  "PROMPT org-completing-read on ENTRIES and call ACTION on selected.
If there is only one entry, call ACTION without completing read.
If there are no entries, raise exception."
  (let* ((entries-count (length entries))
         (choice (cond ((= entries-count 1) (caar entries))
                       ((= entries-count 0) (error "Empty set."))
                       (t (org-completing-read prompt entries))))
         (marker (cadr (assoc-string choice entries)))
         (source-buffer (current-buffer)))

    ;; og-context debug
    ;; (let ((context (og-context :target choice)))
    ;;   (eieio-persistent-save context))
    ;; debug end

    (if save-outline-visibility-p
        (org-save-outline-visibility t
          (org-goto-marker-or-bmk marker)

          (ledna/set-property "CONTEXT" "hello")

          (funcall action))
      (progn
        (org-goto-marker-or-bmk marker)
        (funcall action)))))

(defun org-glance/follow-org-link-at-point ()
  "Browse org-link at point."
  (let ((link (buffer-substring-no-properties
               (save-excursion (org-beginning-of-line) (point))
               (save-excursion (org-end-of-line) (point))))
        (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
    (org-open-link-from-string link)))

(defun org-glance--aggregate-scopes (&optional scopes)
  "Provides list of scopes (scope may be buffer or existing file).
Without specifying SCOPES it returns list with current buffer."

  (let* ((scopes (cond ((or (stringp scopes)
                            (and (symbolp scopes)
                                 (not (null scopes))))
                        (list scopes))
                       (t scopes)))

         (ascopes (cl-loop for scope in scopes

                           ;; collect buffers
                           when (bufferp scope)
                           collect scope

                           ;; collect functions that return buffers or filenames
                           when (functionp scope)
                           collect (when-let ((fob (funcall scope)))
                                     (if (bufferp fob)
                                         fob
                                       (or (get-file-buffer (expand-file-name fob))
                                           (expand-file-name fob))))

                           ;; collect file names
                           when (and (stringp scope) (file-exists-p (expand-file-name scope)))
                           collect (or (get-file-buffer (expand-file-name scope))
                                       (expand-file-name scope)))))

    (or (remove 'nil (seq-uniq ascopes))
        (list (current-buffer)))))

(defvar org-glance/default-filters '((links . (lambda () (org-match-line (format "^.*%s.*$" org-bracket-link-regexp))))
                                     (encrypted . (lambda () (seq-intersection (list "crypt") (org-get-tags-at))))))

(defun org-glance--filter-predicates (filter)
  "Factorize FILTER into list of predicates. Acceptable FILTER values:
- list of symbols (possible default filters) and lambdas (custom filters)
- string name of default filter
- symbolic name of default filter
- lambda function with no params called on entry"
  (cond ((functionp filter) (list filter))
        ((symbolp filter) (list (alist-get filter org-glance/default-filters)))
        ((stringp filter) (list (alist-get (intern filter) org-glance/default-filters)))
        ((listp filter) (cl-loop for elt in filter
                                 when (functionp elt) collect elt
                                 when (symbolp elt)   collect (alist-get elt org-glance/default-filters)
                                 when (stringp elt)   collect (alist-get (intern elt) org-glance/default-filters)))
        (t (error "Unable to recognize filter."))))

(provide 'org-glance)
;;; org-glance.el ends here
