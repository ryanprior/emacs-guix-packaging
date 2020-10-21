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

;; Package-Requires: ((emacs "27.1") (dash "2.17.0") (yasnippet "0.14.0"))

;;; Commentary:

;; emacs-guix-packaging (aka "guix-packaging.el") provides tools to
;; create and maintain Guix packages quickly and with confidence.

;;; Code:

(require 'yasnippet)
(require 'dash)
(require 'rx)

(defgroup guix nil
  "Interface for the GNU Guix package manager."
  :prefix "guix-"
  :group 'external)

(defgroup guix-packaging nil
  "Tools for writing and maintaining Guix packages."
  :prefix "guix-packaging"
  :group 'guix)

(defcustom guix-packaging-slug-dash-pattern (rx (any " " "_" "/" "."))
  "Pattern matching characters to replace with dashes in a slug."
  :type '(regexp)
  :group 'guix-packaging)

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
  (-> current
      (string= "")
      (or (null current))
      not
      (and current)
      (or init)))

;;;###autoload
(defun guix-packaging-go-mod-to-org-checkbox (&optional depth buffer)
  (interactive "p")
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (line-beginning-position))
      (insert (make-string (-> depth (or 1) (* 2)) ? ))
      (insert "- [ ] ")
      (delete-forward-char 1)
      (search-forward " ")
      (delete-backward-char 1)
      (delete-forward-char 1)
      (insert "@"))))

(setq guix-packaging--snippets-root
      (file-name-directory (or load-file-name
                               (buffer-file-name))))

;;;###autoload
(defun guix-packaging--snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" guix-packaging--snippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-load-directory snip-dir)))

;;;###autoload
(with-eval-after-load "yasnippet" (guix-packaging--snippets-initialize))

(provide 'guix-packaging)

;;; guix-packaging.el ends here
