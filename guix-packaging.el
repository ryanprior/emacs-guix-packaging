;;; guix-packaging.el --- Tools for writing and maintaining Guix packages -*- lexical-binding: t; -*-

;; Copyright © 2020 Ryan Prior

;; Author: Ryan Prior <rprior@protonmail.com>
;; Keywords: guix tools snippets
;; Version: 1.0
;; Homepage: https://github.com/ryanprior/emacs-guix-packaging
;; Package-Requires: ((emacs "27.1") (dash "2.17.0") (yasnippet "0.14.0"))

;; This file is not part of GNU Emacs.

;; emacs-guix-packaging is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; guix-packaging (aka "guix-packaging.el") provides tools to create and
;; maintain Guix packages quickly and with confidence.


;; Commands
;; ════════

;; `M-x' `guix-packaging-insert-input'
;; ───────────────────────────────────

;;   Prompts for a package string with completion, for example
;;   `ruby@2.7.2', and inserts the corresponding Guix input form such as
;;   `("ruby@2.7.2" ,ruby-2.7)'. It reads the Guile source code to find the
;;   appropriate symbol name, so it might not work with some packages that
;;   have unusual source code.


;; `M-x' `guix-packaging-hash-git'
;; ───────────────────────────────

;;   Saves the Guix hash to the kill ring for a git repository URL (the one
;;   at point by default) at a given tag.


;; `M-x' `guix-packaging-go-mod-to-checklist-dwim'
;; ───────────────────────────────────────────────

;;   Turns go module definitions into an org/markdown checklist, suitable
;;   to keep track of packaging progress.


;; `M-x' `guix-packaging-go-mod-to-checkbox'
;; ─────────────────────────────────────────

;;   Replace a single go module definition with a checkbox.


;; Snippets
;; ════════

;; scheme mode
;; ───────────

;;   The `guix-go' snippet creates the skeleton of a go module package and
;;   assists you in quickly filling it out. You first provide the import
;;   path, from which the snippet infers the symbol & package names, the
;;   repo URL, the hash, and the homepage. All of these are presented in
;;   the snippet as default values that you can change as you go for
;;   unusual cases.

;;   The `guix-go-noversion' snippet is similar but instead provides a
;;   package skeleton suitable for those modules which don't have any
;;   releases.


;; sgml, web, markdown, and org modes
;; ──────────────────────────────────

;;   The `guix-badge' snippet inserts the HTML for a Repology badge showing
;;   the package status in Guix and linking to the project page in
;;   Repology, handy for project status pages.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'files)
(require 'rx)
(require 'simple)
(require 'subr-x)
(require 'thingatpt)
(require 'yasnippet)

(defgroup guix nil "Interface for the GNU Guix package manager."
  :prefix "guix-"
  :group 'external)

(defgroup guix-packaging nil "Tools for writing and maintaining Guix packages."
  :prefix "guix-packaging-"
  :group 'guix)

(defcustom guix-packaging-output-buffer "*guix-packaging*"
  "Buffer for the output of packaging commands."
  :type '(string)
  :group 'guix-packaging)

(defcustom guix-packaging-error-buffer "*guix-packaging*"
  "Buffer for the error messages of packaging commands."
  :type '(string)
  :group 'guix-packaging)

(defcustom guix-packaging-guix-executable "~/.config/guix/current/bin/guix"
  "The executable of guix."
  :type '((file :must-match t))
  :group 'guix-packaging)

(defcustom guix-packaging-slug-dash-pattern (rx (any " " "_" "/" "."))
  "Pattern matching characters to replace with dashes in a slug."
  :type '(regexp)
  :group 'guix-packaging)

(defcustom guix-packaging-go-mod-pattern
  (rx line-start
      (* space)
      (+ (not space))
      "/"
      (+ (not space))
      " "
      (+ (not space))
      (* not-newline)
      line-end)
  "Pattern matching a single go module requirement."
  :type '(regexp)
  :group 'guix-packaging)

(defcustom guix-packaging-go-mod-start-pattern
  (rx line-start
      (* space)
      "require"
      (+ space)
      "("
      (* space)
      line-end)
  "Pattern matching the beginning of a go module requirement block."
  :type '(regexp)
  :group 'guix-packaging)

(defcustom guix-packaging-go-mod-end-pattern
  (rx line-start
      (* space)
      ")"
      (* space)
      line-end)
  "Pattern matching the end of a go module requirement block."
  :type '(regexp)
  :group 'guix-packaging)

(defconst guix-packaging--snippets-root
  (file-name-directory (or load-file-name
                           (buffer-file-name)))
  "Directory where guix-packaging snippets reside.")

(defun guix-packaging--message (&rest args)
  "Insert ARGS into the `guix-packaging-output-buffer'."
  (with-current-buffer (get-buffer-create guix-packaging-output-buffer)
    (save-excursion
      (goto-char (point-max))
      (-each args #'insert)
      (insert "\n"))))

(defun guix-packaging--make-slug (string)
  "Make a slug out of STRING.
Replaces whitespaces, dots, slashes & underscores in STRING with
dashes and removes other non-alphanumeric characters to make a
slug suitable as a bland Lisp or scheme symbol."
  (->> string
       downcase
       (replace-regexp-in-string (rx (+ (regexp guix-packaging-slug-dash-pattern)))
                                 "-")
       (replace-regexp-in-string (rx (not (any alphanumeric "-")))
                                 "")))

(defmacro guix-packaging--latch (current init)
  "CURRENT unless it's nil or an empty string, in which case INIT."
  `(if (or (string= "" ,current)
           (null ,current))
       ,init
     ,current))

(defun guix-packaging--goto-line (n &optional buffer)
  "Go to the Nth line."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (forward-line (1- n))))

(defun guix-packaging--do-on-each-line (func &optional start end)
  "Run a command on each line.
Move point to each line between START and END (or current
selected region) and run FUNC each time."
  (let ((start (line-number-at-pos (or start
                                       (region-beginning))))
        (end (line-number-at-pos (or end
                                     (region-end)))))
    (save-mark-and-excursion
      (set-mark nil)
      (guix-packaging--goto-line start)
      (while (<= (line-number-at-pos) end)
        (funcall func)
        (forward-line)))))

(defun guix-packaging--tsv-to-plist (tsv-string)
  "Transform a TSV-STRING for a package into a plist."
  (let* ((fields (split-string tsv-string "\t"))
         (outputs (split-string (nth 2 fields) ","))
         (location (-zip '(:file :line :char)
                         (split-string (nth 3 fields) ":"))))
    `(:name ,(nth 0 fields)
            :version ,(nth 1 fields)
            :outputs ,outputs
            :location ,location)))

(defun guix-packaging--map-tsv-to-plist (package-strings)
  "Transform tsv PACKAGE-STRINGS to plists."
  (map 'list #'guix-packaging--tsv-to-plist package-strings))

(defun guix-packaging--list-available (search-regex)
  "Available packages in Guix matching SEARCH-REGEX, in a plist."
  (-> "%s package -A %s"
      (format guix-packaging-guix-executable search-regex)
      shell-command-to-string
      (split-string "\n" t)
      guix-packaging--map-tsv-to-plist))

(defun guix-packaging--add-rec-field (plist pair)
  "Add record PAIR to PLIST."
  (plist-put plist
             (intern (concat ":" (first pair)))
             (second pair)))

(defun guix-packaging--rec-to-plist (rec-string)
  "Transfrom a REC-STRING for a package into a plist."
  (let* ((reduced-string (replace-regexp-in-string "\n[+]" " " rec-string))
         (fields (--> reduced-string
                      (split-string it "\n")
                      (remove-if #'string-empty-p it)
                      (map 'list (-rpartial #'split-string ": ") it)
                      (reduce #'guix-packaging--add-rec-field
                              it
                              :initial-value '())))
         (dependencies (split-string (plist-get fields :dependencies))))
    (plist-put fields :dependencies
               dependencies)))

(defun guix-packaging--package (name)
  "Info about the package NAME in a plist."
  (--> "%s show %s"
       (format it guix-packaging-guix-executable name)
       shell-command-to-string
       guix-packaging--rec-to-plist))

(defun guix-packaging--guile-symbol (package-string)
  "The Guile symbol for PACKAGE-STRING."
  (let* ((package-location (->> package-string
                               (format "VISUAL=echo %s edit %s" guix-packaging-guix-executable)
                               shell-command-to-string))
         (data-pair (split-string package-location))
         (line (-> data-pair
                   first
                   (string-trim "+")
                   string-to-number))
         (file-path (second data-pair)))
      (with-current-buffer (get-buffer-create "*pkg*")
        (erase-buffer)
        (insert-file-contents file-path)
        (guix-packaging--goto-line line (current-buffer))
        (search-backward "define-public")
        (end-of-line)
        (scheme-mode)
        (thing-at-point 'symbol t))))

(defun guix-packaging--format-input (package-string)
  "Format PACKAGE-STRING as a Guix package input."
  (format "(\"%s\" ,%s)" package-string (guix-packaging--guile-symbol package-string)))

(defun guix-packaging--make-package-string (package)
  "The package-string (name@version) for PACKAGE."
  (format "%s@%s"
          (plist-get package :name)
          (plist-get package :version)))

;;;###autoload
(defun guix-packaging-insert-input (package-string)
  "Insert the corresponding Guix package input for PACKAGE-STRING.
eg. for ruby@2.7.2 insert (\"ruby@2.7.2\" ,ruby-2.7)."
  (interactive
   (let ((package-string
          (completing-read "Search packages: "
                           (completion-table-dynamic
                            (lambda (query)
                              "Get names of packages that match STRING."
                              (map 'list #'guix-packaging--make-package-string
                                   (guix-packaging--list-available query)))))))
     (list package-string)))
  (insert (guix-packaging--format-input package-string)))

;;;###autoload
(defun guix-packaging-go-mod-to-checkbox (&optional depth)
  "Convert a go module requirement to a checkbox.
Prepend 2 times DEPTH spaces, make a list item with a checkbox,
and use the go module requirement as the label."
  (interactive "p")
  (save-excursion
    (goto-char (line-beginning-position))
    (insert (-> depth
                (or 1)
                (* 2)
                (make-string ? )))
    (insert "- [ ]")
    (fixup-whitespace)
    (forward-char)
    (search-forward " ")
    (delete-char -1)
    (delete-char 1)
    (insert "@")))

(defun guix-packaging--go-mod-region-to-checkboxes (&optional depth)
  "Convert the region from go module requirements a checklist.
Prepend 2 times DEPTH spaces before each list element."
  (guix-packaging--do-on-each-line
   (lambda ()
     "Convert to checkbox with given DEPTH and BUFFER."
     (guix-packaging-go-mod-to-checkbox depth))))

(defun guix-packaging--mark-go-mod ()
  "Mark go module requirements.
Use the current region or look around point in the current
buffer. Use `guix-packaging-go-mod-pattern' to identify target
lines."
  (let ((start (line-number-at-pos (if (use-region-p)
                                       (region-beginning)
                                     (point))))
        (region-min-line-number nil)
        (in-mod-region (save-mark-and-excursion
                         (beginning-of-line)
                         (looking-at-p guix-packaging-go-mod-pattern)))
        (max-point (buffer-size)))
    (when in-mod-region
      (beginning-of-line)
      (while (and (not (eq (point) 1))
                  (looking-at-p guix-packaging-go-mod-pattern))
        (forward-line -1))
      (when (not (looking-at-p guix-packaging-go-mod-pattern))
        (forward-line))
      (setq region-min-line-number (line-number-at-pos))
      (push-mark (point) t t)
      (guix-packaging--goto-line start)
      (while (and (not (eq (point) max-point))
                  (looking-at-p guix-packaging-go-mod-pattern))
        (forward-line))
      (when (not (looking-at-p guix-packaging-go-mod-pattern))
        (forward-line -1))
      (end-of-line)
      `(,region-min-line-number ,(line-number-at-pos)))))

;;;###autoload
(defun guix-packaging-go-mod-to-checklist-dwim (&optional depth buffer)
  "Convert the region of go module requirements to a checklist.
Prepend 2 times DEPTH spaces before each list item. Use the
region from BUFFER, or if no region is selected, widen to the go
mod block at point."
  (interactive "p")
  (if (use-region-p)
      (guix-packaging--go-mod-region-to-checkboxes depth)
    (destructuring-bind (&optional start end)
        (guix-packaging--mark-go-mod)
      (when (and start end)
        (with-current-buffer (or buffer (current-buffer))
          (save-mark-and-excursion
            (guix-packaging--go-mod-region-to-checkboxes depth)
            (guix-packaging--goto-line (- start 1))
            (beginning-of-line)
            (when (looking-at-p guix-packaging-go-mod-start-pattern)
              (delete-region (point)
                             (1+ (line-end-position))))
            (guix-packaging--goto-line (1+ end))
            (beginning-of-line)
            (when (looking-at-p guix-packaging-go-mod-end-pattern)
              (delete-region (point)
                             (1+ (min (buffer-size)
                                      (line-end-position)))))))))))

(defun guix-packaging--tmp-repo-dir (repo-url)
  "The name of a temporary directory for REPO-URL."
  (format "/tmp/%s" (guix-packaging--make-slug repo-url)))

(defun guix-packaging--git-clone-tmp (repo-url &optional branch)
  "Clone the git repository with the provided REPO-URL and BRANCH to a temporary directory."
  (let* ((shell-command-dont-erase-buffer t)
         (branch-options (when branch
                           (format "--branch \"%s\" " branch)))
         (dest (guix-packaging--tmp-repo-dir repo-url))
         (cmd (format "git clone --depth=1 %s%s %s"
                      branch-options
                      repo-url
                      dest)))
    (when (file-directory-p dest)
      (guix-packaging--message "Removing existing " dest)
      (delete-directory dest t))
    (guix-packaging--message "$ " cmd)
    (shell-command cmd guix-packaging-output-buffer
                   guix-packaging-error-buffer)))

;;;###autoload
(defun guix-packaging-hash-git (&optional repo-url branch)
  "Save the hash of the git repository at REPO-URL to the kill ring.
If BRANCH provided, git uses that branch (or tag.)"
  (interactive
   (let* ((default (thing-at-point 'url))
          (repo-url (read-string (concat "Repository URL"
                                         (when default
                                           (concat " (default " default ")"))
                                         ": ")
                                 nil nil default))
          (branch (read-string "Branch (default master): "
                               nil nil "master")))
     (list repo-url branch)))
  (if (zerop (guix-packaging--git-clone-tmp repo-url branch))
      (->> repo-url
           guix-packaging--tmp-repo-dir
           (format "%s hash -rx %s" guix-packaging-guix-executable)
           shell-command-to-string
           string-trim-right
           kill-new
           message)
    (when (called-interactively-p 'interactive)
      (message "Couldn't hash %s at branch %s. See %s for info."
               (propertize repo-url 'face 'link)
               branch
               (propertize guix-packaging-error-buffer 'face
                           'error)))
    nil))

;;;###autoload
(defun guix-packaging--snippets-initialize ()
  "Initialize yasnippet to use the guix-packaging snippets."
  (let ((snip-dir (expand-file-name "snippets" guix-packaging--snippets-root)))
    (when (boundp #'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-load-directory snip-dir)))

;;;###autoload
(with-eval-after-load "yasnippet"
  (guix-packaging--snippets-initialize))

(provide 'guix-packaging)

;;; guix-packaging.el ends here
