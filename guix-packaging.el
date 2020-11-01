;;; guix-packaging.el --- Tools for writing and maintaining Guix packages

;; Copyright © 2020 Ryan Prior

;; Author: Ryan Prior <rprior@protonmail.com>
;; Keywords: guix tools snippets

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

;; Package-Requires: ((emacs "27.1") (cl-lib "0.6.1") (dash "2.17.0") (files) (rx) (simple) (subr-x) (thingatpt) (yasnippet "0.14.0"))

;;; Commentary:

;; emacs-guix-packaging (aka "guix-packaging.el") provides tools to
;; create and maintain Guix packages quickly and with confidence.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'files)
(require 'rx)
(require 'simple)
(require 'subr-x)
(require 'thingatpt)
(require 'yasnippet)

(defgroup guix nil
  "Interface for the GNU Guix package manager."
  :prefix "guix-"
  :group 'external)

(defgroup guix-packaging nil
  "Tools for writing and maintaining Guix packages."
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

(defcustom guix-packaging-slug-dash-pattern (rx (any " " "_" "/" "."))
  "Pattern matching characters to replace with dashes in a slug."
  :type '(regexp)
  :group 'guix-packaging)

(defcustom guix-packaging-go-mod-pattern (rx line-start (* space)
                               (+ (not space)) "/"
                               (+ (not space)) " "
                               (+ (not space))
                               (* not-newline)
                               line-end)
  "Pattern matching a single go module requirement."
  :type '(regexp)
  :group 'guix-packaging)

(defcustom guix-packaging-go-mod-start-pattern (rx line-start (* space)
                                     "require" (+ space)
                                     "(" (* space)
                                     line-end)
  "Pattern matching the beginning of a go module requirement block."
  :type '(regexp)
  :group 'guix-packaging)

(defcustom guix-packaging-go-mod-end-pattern (rx line-start (* space)
                                   ")" (* space)
                                   line-end)
  "Pattern matching the end of a go module requirement block."
  :type '(regexp)
  :group 'guix-packaging)

(setq guix-packaging--snippets-root
      (file-name-directory (or load-file-name
                               (buffer-file-name))))

(defun guix-packaging--message (&rest args)
  (with-current-buffer guix-packaging-output-buffer
    (-each args 'insert)
    (insert "\n")))

(defun guix-packaging--make-slug (string)
  "Replaces whitespaces, dots, slashes & underscores in STRING
  with dashes and removes other non-alphanumeric characters to
  make a slug suitable as a bland lisp or scheme symbol."
  (->> string
       downcase
       (replace-regexp-in-string
        (rx (+ (regexp guix-packaging-slug-dash-pattern)))
        "-")
       (replace-regexp-in-string
        (rx (not (any alphanumeric "-")))
        "")))

(defun guix-packaging--latch (current init)
  "CURRENT unless it's nil or an empty string, in which case INIT."
  (if (or (string= "" current)
          (null current))
      init
    current))

(defun guix-packaging--do-on-each-line (func &optional start end buffer)
  "Move point to each line between START and END (or current
  selected region) and run FUNC."
  (let ((start (line-number-at-pos (or start (region-beginning))))
        (end (line-number-at-pos (or end (region-end))))
        (buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (set-mark nil)
        (goto-line start)
        (while (<= (line-number-at-pos) end)
          (funcall func)
          (forward-line))))))

;;;###autoload
(defun guix-packaging-go-mod-to-checkbox (&optional depth buffer)
  "Convert the line at point from a go module requirement to an
org/markdown checkbox list item."
  (interactive "p")
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (line-beginning-position))
      (insert (make-string (-> depth (or 1) (* 2)) ? ))
      (insert "- [ ]")
      (fixup-whitespace)
      (forward-char)
      (search-forward " ")
      (delete-backward-char 1)
      (delete-forward-char 1)
      (insert "@"))))

(defun guix-packaging--go-mod-region-to-checkboxes (&optional depth buffer)
  "Convert all lines in the current region from go module
requirements to org/markdown checklist items using
guix-packaging-go-mod-to-checkbox."
  (guix-packaging--do-on-each-line
   (lambda ()
     "Convert to checkbox with given DEPTH and BUFFER."
     (guix-packaging-go-mod-to-checkbox depth buffer))))

(defun guix-packaging--widen-go-mod (&optional buffer)
  "Widen region to contain all contiguous lines which match
  guix-packaging-go-mod-pattern."
  (let ((start (line-number-at-pos (if (use-region-p) (region-beginning) (point))))
        (region-min-line-number nil)
        (in-mod-region (save-mark-and-excursion
                         (beginning-of-line)
                         (looking-at-p guix-packaging-go-mod-pattern)))
        (max-point (buffer-size))
        (buffer (or buffer (current-buffer))))
    (when in-mod-region
      (beginning-of-line)
      (while (and (not (eq (point) 1))
                  (looking-at-p guix-packaging-go-mod-pattern))
        (previous-line))
      (when (not (looking-at-p guix-packaging-go-mod-pattern))
        (forward-line))
      (setq region-min-line-number (line-number-at-pos))
      (push-mark (point) t t)
      (goto-line start)
      (while (and (not (eq (point) max-point))
                  (looking-at-p guix-packaging-go-mod-pattern))
        (forward-line))
      (when (not (looking-at-p guix-packaging-go-mod-pattern))
        (previous-line))
      (end-of-line)
      `(,region-min-line-number ,(line-number-at-pos)))))

;;;###autoload
(defun guix-packaging-go-mod-to-checklist-dwim (&optional depth buffer)
  "Convert all lines in the current region from go module
  requirements to org/markdown checklist items using
  guix-packaging-go-mod-to-checkbox, or if no region is selected,
  convert the go mod block at point."
  (interactive "p")
  (if (use-region-p)
      (begin (guix-packaging--go-mod-region-to-checkboxes depth buffer) (message "used region"))
    (destructuring-bind (&optional start end) (guix-packaging--widen-go-mod buffer)
      (when (and start end)
        (save-mark-and-excursion
          (guix-packaging--go-mod-region-to-checkboxes depth buffer)
          (goto-line (- start 1))
          (beginning-of-line)
          (when (looking-at-p guix-packaging-go-mod-start-pattern)
            (delete-region (point) (+ (line-end-position) 1)))
          (goto-line (+ end 1))
          (beginning-of-line)
          (when (looking-at-p guix-packaging-go-mod-end-pattern)
            (delete-region (point) (+ (min (buffer-size) (line-end-position)) 1))))))))

(defun guix-packaging--tmp-repo-dir (repo-url)
  "The name of a temporary directory for REPO-URL."
  (concat "/tmp/" (guix-packaging--make-slug repo-url)))

(defun guix-packaging--git-clone-tmp (repo-url &optional branch)
  "Clone the git repository with the provided REPO-URL to a temporary directory."
  (let* ((shell-command-dont-erase-buffer t)
         (branch-options (when branch (concat "--branch \"" branch "\" ")))
         (dest (guix-packaging--tmp-repo-dir repo-url))
         (cmd (concat "git clone --depth=1 "
                      branch-options
                      repo-url
                      " " dest)))
    (when (file-directory-p dest)
      (guix-packaging--message "Removing existing " dest)
      (delete-directory dest t))
    (guix-packaging--message "$ " cmd)
    (shell-command cmd
                   guix-packaging-output-buffer
                   guix-packaging-error-buffer)))

;;;###autoload
(defun guix-packaging-hash-git (&optional repo-url branch)
  "Save the hash of the git repository at REPO-URL to the kill
  ring. If BRANCH provided, git uses that branch (or tag.)"
  (interactive
   (let* ((default (thing-at-point 'url))
          (repo-url (read-string
                     (concat "Repository URL"
                             (when default (concat " (default " default ")") )
                             ": ")
                     nil nil
                     default))
          (branch (read-string "Branch (default master): " nil nil "master")))
     (list repo-url branch)))
  (guix-packaging--git-clone-tmp repo-url branch)
  (->> repo-url
       guix-packaging--tmp-repo-dir
       (concat "guix hash -rx ")
       shell-command-to-string
       string-trim-right
       kill-new
       message))

;;;###autoload
(defun guix-packaging--snippets-initialize ()
  "Initialize yasnippet to use the guix-packaging snippets."
  (let ((snip-dir (expand-file-name "snippets" guix-packaging--snippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-load-directory snip-dir)))

;;;###autoload
(with-eval-after-load "yasnippet" (guix-packaging--snippets-initialize))

(provide 'guix-packaging)

;;; guix-packaging.el ends here
