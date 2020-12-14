;;; guix-packaging.el --- Tools for writing and maintaining Guix packages -*- lexical-binding: t; -*-

;; Copyright © 2020 Ryan Prior

;; Author: Ryan Prior <rprior@protonmail.com>
;; Keywords: guix tools snippets
;; Version: 1.5
;; Homepage: https://github.com/ryanprior/emacs-guix-packaging
;; Package-Requires: ((emacs "27.1") (dash "2.17.0") (dash-functional "1.2.0") (yasnippet "0.14.0") (seq "2.22") (pulse "1.0"))

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

;;   Prompts for a set of package strings with completion, for example
;;   `ruby@2.7.2', and inserts the corresponding Guix input form such as
;;   `("ruby@2.7.2" ,ruby-2.7)'.

;;   Reads the Guile source code to find the appropriate symbol name, so it
;;   might not work with some packages that have unusual source code.


;; `M-x' `guix-packaging-hash-git'
;; ───────────────────────────────

;;   Saves the Guix hash to the kill ring for a git repository URL at a
;;   given tag.

;;   Reads your mind to get the default URL: prefers the URL at point,
;;   falls back to the URL following `(url "' in the `defun' at point, or
;;   as a last resort, uses the next match in the buffer for
;;   `goto-address-url-regexp'.


;; `M-x' `guix-packaging-go-mod-to-checklist-dwim'
;; ───────────────────────────────────────────────

;;   Turns go module definitions into an org/markdown checklist, suitable
;;   to keep track of packaging progress.


;; `M-x' `guix-packaging-go-mod-to-checkbox'
;; ─────────────────────────────────────────

;;   Replace a single go module definition with a checkbox.

;; `M-x' `guix-packaging-refresh-packages'
;; ───────────────────────────────────────

;;   Refresh the cache of information about available Guix packages.


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


;; text mode
;; ─────────

;;   The `guix-issue-url' snippet inserts a URL pointing to the Guix issue
;;   tracker.



;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 'files)
(require 'gv)
(require 'pulse)
(require 'rx)
(require 'seq)
(require 'simple)
(require 'subr-x)
(require 'thingatpt)
(require 'yasnippet)

(gv-define-simple-setter plist-get plist-put)

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

(defcustom guix-packaging-extra-load-paths nil
  "Extra directories to add to the Guile load path when invoking Guix."
  :type '(repeat directory)
  :group 'guix-packaging)

(defcustom guix-packaging-slug-dash-pattern (rx (any " " "_" "/" "."))
  "Pattern matching characters to replace with dashes in a slug."
  :type '(regexp)
  :group 'guix-packaging)

(defconst guix-packaging-go-mod-pattern
  (rx line-start
      (* space)
      (+ (not space))
      "/"
      (+ (not space))
      " "
      (+ (not space))
      (* not-newline)
      line-end)
  "Pattern matching a single go module requirement.")

(defconst guix-packaging-go-mod-start-pattern
  (rx line-start
      (* space)
      "require"
      (+ space)
      "("
      (* space)
      line-end)
  "Pattern matching the beginning of a go module requirement block.")

(defconst guix-packaging-go-mod-end-pattern
  (rx line-start
      (* space)
      ")"
      (* space)
      line-end)
  "Pattern matching the end of a go module requirement block.")

(defconst guix-packaging--snippets-root
  (file-name-directory (or load-file-name
                           (buffer-file-name)))
  "Directory where guix-packaging snippets reside.")

(defconst guix-packaging--data-dir
  (concat user-emacs-directory "/var/guix-packaging/")
  "Directory for saving Guix package related data.")
(mkdir guix-packaging--data-dir t)

(defvar guix-packaging--strategies (make-hash-table :test #'equal)
  "Hash containing known strategies for Guix packages.")
(defvar guix-packaging--save-after-remember t)

(defconst guix-packaging--no-load-path-commands
  '("hash"))

(defvar guix-packaging--all-guix-packages nil
  "List containing all packages in the local Guix.")



(cl-defun guix-packaging--message (msg &key prefix)
  "Insert MSG into the `guix-packaging-output-buffer', optionally preceeded by PREFIX."
  (with-current-buffer (get-buffer-create guix-packaging-output-buffer)
    (save-excursion
      (goto-char (point-max))
      (when prefix (insert prefix))
      (insert msg)
      (newline)
      msg)))

(cl-defun guix-packaging--invoke-guix (cmd &rest (args (cl-rest (split-string cmd))))
  (let ((load-strings (cl-map #'list (-partial #'format "-L \"%s\"") guix-packaging-extra-load-paths))
        (cmd (if args
                 cmd
               (cl-first (split-string cmd)))))
    (thread-first (if (-contains-p guix-packaging--no-load-path-commands cmd)
                      (list guix-packaging-guix-executable cmd args)
                    (list guix-packaging-guix-executable cmd load-strings args))
      flatten-list
      (string-join " ")
      (guix-packaging--message :prefix "$ ")
      (concat " 2>/dev/null")
      shell-command-to-string)))

(defun guix-packaging--make-slug (string)
  "Make a slug out of STRING.
Replaces whitespaces, dots, slashes & underscores in STRING with
dashes and removes other non-alphanumeric characters to make a
slug suitable as a bland Lisp or scheme symbol."
  (thread-last string
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

(cl-defun guix-packaging--goto-line (n &optional (buffer (current-buffer)))
  "Go to the Nth line."
  (with-current-buffer buffer
    (goto-char (point-min))
    (forward-line (1- n))))

(cl-defun guix-packaging--do-on-each-line (func &optional (start (region-beginning)) (end (region-end)))
  "Run a command on each line.
Move point to each line between START and END (or current
selected region) and run FUNC each time."
  (let ((start (line-number-at-pos start))
        (end (line-number-at-pos end)))
    (save-mark-and-excursion
      (set-mark nil)
      (guix-packaging--goto-line start)
      (while (<= (line-number-at-pos) end)
        (funcall func)
        (forward-line)))))

(defmacro guix-packaging--with-scheme-buffer (&rest body)
  "Evaluate BODY using a temporary scheme-mode buffer."
  (declare (indent 0))
  `(let ((geiser-mode-auto-p nil))
     (with-temp-buffer
       (font-lock-mode -1)
       (scheme-mode)
       ,@body)))

(defun guix-packaging--pulse-region (start end)
  (when (pulse-available-p)
    (pulse-momentary-highlight-region start end)))




(defun thing-at-point--beginning-of-go-mod ()
  "Go to the beginning of a go mod declaration."
  (goto-char (line-beginning-position))
  (unless (looking-at-p guix-packaging-go-mod-pattern)
    (error "No go mod here"))
  (point))

(defun thing-at-point--end-of-go-mod ()
  "Go to the end of a go mod declaration."
  (goto-char (line-end-position))
  (unless (looking-back guix-packaging-go-mod-pattern (line-beginning-position))
    (error "No go mod here"))
  (point))

(defun thing-at-point--bounds-of-go-mod-at-point ()
  "Get boundaries of go mod declaration at point."
  (save-excursion
    (let* ((beg (thing-at-point--beginning-of-go-mod))
           (end (thing-at-point--end-of-go-mod)))
      (cons beg end))))

(defun thing-at-point-go-mod-at-point ()
  "Get go mod declaration at point."
  (let ((bounds (bounds-of-thing-at-point 'go-mod)))
    (buffer-substring (car bounds) (cdr bounds))))

(put 'go-mod 'beginning-op #'thing-at-point--beginning-of-go-mod)
(put 'go-mod 'end-op #'thing-at-point--end-of-go-mod)
(put 'go-mod 'bounds-of-thing-at-point #'thing-at-point--bounds-of-go-mod-at-point)
(put 'go-mod 'thing-at-point #'thing-at-point-go-mod-at-point)



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
  (cl-map #'list #'guix-packaging--tsv-to-plist package-strings))

(defun guix-packaging--pairs-to-plist (pairs &optional plist)
  "Add record PAIRS to PLIST."
  (cl-reduce (lambda (plist pair)
              "Add PAIR to PLIST."
              (plist-put plist
                         (intern (concat ":" (cl-first pair)))
                         (cl-second pair)))
            pairs
            :initial-value plist))

(defun guix-packaging--rec-to-plist (rec-string)
  "Transfrom a REC-STRING for a package into a plist."
  (let* ((reduced-string (replace-regexp-in-string (rx "\n" "+") " " rec-string))
         (fields (thread-last (split-string reduced-string "\n" t)
                   (cl-map #'list (-rpartial #'split-string ": "))
                   guix-packaging--pairs-to-plist))
         (dependencies (split-string (plist-get fields :dependencies))))
    (plist-put fields :dependencies dependencies)))

(defun guix-packaging--defun-symbol ()
  "Symbol that names the current defun."
  (save-excursion
    (beginning-of-thing 'defun)
    (goto-char (line-end-position))
    (thing-at-point 'symbol t)))

(defun guix-packaging--guile-symbols (&rest package-strings)
  "Hash of PACKAGE-STRINGS to their corresponding Guile symbols."
  (let* ((guix-packaging-guix-executable (concat "VISUAL=echo "guix-packaging-guix-executable))
         (package-locations (thread-last (apply #'guix-packaging--invoke-guix "edit" package-strings)
                              split-string
                              (-partition 2))))
    (cl-reduce
     (lambda (hash next)
       "Add Guile symbol NEXT data pair to HASH."
       (let* ((package-string (cl-first next))
              (data-pair (cl-rest next))
              (line (thread-first data-pair
                      cl-first
                      (string-trim "+")
                      string-to-number))
              (file-path (cl-second data-pair)))
         (guix-packaging--with-scheme-buffer
           (insert-file-contents file-path)
           (guix-packaging--goto-line line (current-buffer))
           (puthash package-string (guix-packaging--defun-symbol) hash)
           hash)))
     (-zip package-strings package-locations)
     :initial-value (make-hash-table))))

(defun guix-packaging--disassemble-package ()
  "Disassemble the package in the defun at point."
    (let ((defun-end (progn
                       (end-of-defun)
                       (1- (line-number-at-pos))))
          (result `(:symex ,(make-hash-table) :strategy (:parts nil))))
      (beginning-of-defun)
      (search-forward "(package")
      (while (< (line-number-at-pos) defun-end)
        (forward-sexp)
        (-let* ((body (thing-at-point 'sexp t))
                (name (guix-packaging--with-scheme-buffer
                        (insert body)
                        (goto-char (point-min))
                        (forward-symbol 1)
                        (thing-at-point 'symbol t)))
                (name-symbol (intern (format ":%s" name)))
                ((&plist :strategy (&plist :parts)) result))
          (push name-symbol (plist-get (plist-get result :strategy) :parts))
          (puthash name-symbol body (plist-get result :symex))))
      (cl-callf reverse (plist-get (plist-get result :strategy) :parts))
      result))

(defun guix-packaging--rename-section (section name)
  "Rename SECTION by replacing its first symbol with NAME.
If NAME is nil, don't do anything."
  (if (null name)
      section
    (guix-packaging--with-scheme-buffer
      (insert section)
      (goto-char (point-min))
      (forward-symbol 1)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point 'symbol)
        (goto-char start)
        (delete-forward-char (- end start)))
      (insert name)
      (buffer-string))))

(defun guix-packaging--alias-reverse (aliases)
  "Transform ALIASES such that they represent the reverse substitutions.
For example, '(:greeting \"hello\" :name \"world\")
becomes:     '(:hello \"greeting\" :world \"name\")"
  (cl-loop for (key val) on aliases by #'cddr
          collect (intern (format ":%s" val))
          collect (string-trim (format "%s" key) ":" "")))

(defun guix-packaging--assemble-package (name symex strategy)
  "Assemble a package definition from NAME and SYMEX.
If STRATEGY is a plist with :sections corresponding to a list of
  symbols, sections of the package will appear in the specififed
  order."
  (let* ((all-keys (hash-table-keys symex))
         (part-keys (plist-get strategy :parts))
         (strategized-keys (-intersection part-keys all-keys))
         (unknown-keys (-difference part-keys strategized-keys))
         (other-keys (-difference all-keys part-keys))
         (aliases (plist-get strategy :aliases))
         (sections (cl-map
                    #'list
                    (lambda (part)
                      "Fetch PART from symex hash and maybe rename it."
                      (thread-first part (gethash symex) (guix-packaging--rename-section (plist-get aliases part))))
                          (append strategized-keys unknown-keys other-keys))))
    (guix-packaging--with-scheme-buffer
      (insert (format "(define-public %s\n  (package\n    %s))"
                      name
                      (mapconcat #'identity sections "\n    ")))
      (buffer-string))))



(cl-defun guix-packaging--list-available (&optional (search-regex ""))
  "Available packages in Guix matching SEARCH-REGEX, in a plist."
  (thread-first (guix-packaging--invoke-guix "package" "-A" search-regex)
    (split-string "\n" t)
    guix-packaging--map-tsv-to-plist))

;;;###autoload
(defun guix-packaging-refresh-packages ()
  "Refresh the list of available Guix packages."
  (interactive)
  (setq guix-packaging--all-guix-packages
        (guix-packaging--list-available)))

(defun guix-packaging--package (name &optional extra)
  "Info about the package NAME in a plist.
If EXTRA is non-nil, fetch extra package info using `guix
search'."
  (if extra
      (guix-packaging--rec-to-plist (guix-packaging--invoke-guix "show" name))
    (seq-find (-rpartial #'plist-get :name) guix-packaging--all-guix-packages)))

(defun guix-packaging--format-inputs (&rest package-strings)
  "Format PACKAGE-STRINGS as Guix package inputs."
  (let ((guile-symbols (apply #'guix-packaging--guile-symbols package-strings)))
    (cl-map #'list
           (lambda (package-string)
             "Format PACKAGE-STRING as a Guix package input."
             (format "(\"%s\" ,%s)"
                     package-string
                     (gethash package-string guile-symbols)))
           package-strings)))

(defun guix-packaging--make-package-string (package)
  "The package-string (name@version) for PACKAGE."
  (format "%s@%s"
          (plist-get package :name)
          (plist-get package :version)))

;;;###autoload
(defun guix-packaging-insert-input (&rest package-strings)
  "Insert the corresponding Guix package inputs for PACKAGE-STRINGS.
eg. for ruby@2.7.2 insert (\"ruby@2.7.2\" ,ruby-2.7)."
  (interactive
   (completing-read-multiple
    "Search packages: "
    (cl-map #'list #'guix-packaging--make-package-string
           (or guix-packaging--all-guix-packages
               (guix-packaging-refresh-packages)))))
  (dolist (input (apply #'guix-packaging--format-inputs package-strings))
    (insert input)
    (newline-and-indent)))

;;;###autoload
(cl-defun guix-packaging-go-mod-to-checkbox (&optional (depth 1))
  "Convert a go module requirement to a checkbox.
Prepend 2 times DEPTH spaces, make a list item with a checkbox,
and use the go module requirement as the label."
  (interactive "p")
  (save-excursion
    (goto-char (line-beginning-position))
    (insert (thread-first depth
              (* 2)
              (make-string ? )))
    (insert "- [ ]")
    (org-cycle-list-bullet (mod depth 3))
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
(cl-defun guix-packaging-go-mod-to-checklist-dwim (&optional depth (buffer (current-buffer)))
  "Convert the region of go module requirements to a checklist.
Prepend 2 times DEPTH spaces before each list item. Use the
region from BUFFER, or if no region is selected, widen to the go
mod block at point."
  (interactive "p")
  (if (use-region-p)
      (guix-packaging--go-mod-region-to-checkboxes depth)
    (cl-destructuring-bind (&optional start end)
        (guix-packaging--mark-go-mod)
      (when (and start end)
        (with-current-buffer buffer
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
         (cmd (format "git clone -q --depth=1 %s%s %s"
                      branch-options
                      repo-url
                      dest)))
    (when (file-directory-p dest)
      (guix-packaging--message (format "Removing existing %s" dest))
      (delete-directory dest t))
    (guix-packaging--message cmd :prefix "$ ")
    (shell-command cmd guix-packaging-output-buffer
                   guix-packaging-error-buffer)))

(defun guix-packaging--url-read-my-mind ()
  "The URL around point, or the first URL of the defun at point, if any."
  (save-excursion
    (or (thing-at-point 'url)
        (-when-let* ((fn (bounds-of-thing-at-point 'defun))
                     (beginning (car fn))
                     (end (cdr fn)))
          (goto-char beginning)
          (search-forward-regexp
           (rx "(url" (* space) "\"")
           end)
          (thing-at-point 'url))
        (progn
          (search-forward-regexp goto-address-url-regexp)
          (thing-at-point 'url)))))

;;;###autoload
(defun guix-packaging-hash-git (&optional repo-url branch)
  "Save the hash of the git repository at REPO-URL to the kill ring.
If BRANCH provided, git uses that branch (or tag.)"
  (interactive
   (let* ((default (guix-packaging--url-read-my-mind))
          (repo-url (read-string (concat "Repository URL"
                                         (when default
                                           (concat " (default " default ")"))
                                         ": ")
                                 nil nil default))
          (branch (read-string "Branch (default master): "
                               nil nil "master")))
     (list repo-url branch)))
  (if (zerop (guix-packaging--git-clone-tmp repo-url branch))
      (thread-last repo-url
        guix-packaging--tmp-repo-dir
        (guix-packaging--invoke-guix "hash" "-rx")
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
