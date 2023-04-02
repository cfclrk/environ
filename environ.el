;;; environ.el --- Utilities for setting env variables -*- lexical-binding: t; -*-

;; Author: Chris Clark <cfclrk@gmail.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.17.0") (f "0.20.0") (s "1.12.0"))
;; URL: https://github.com/cfclrk/env

;; This program is free software; you can redistribute it and/or modify
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

;; See the README.md in this package, which is also visible at:
;; https://github.com/cfclrk/env

;;; Code:

(require 'dash)
(require 'f)
(require 's)

;;; Options

(defgroup env ()
  "Utilities to set and unset environment variables in Emacs."
  :group 'environment
  :prefix "environ-"
  :link '(url-link :tag "GitHub" "https://github.com/cfclrk/env"))

(defconst environ-project-dir
  (file-name-directory (if load-in-progress
                           load-file-name
                         (buffer-file-name)))
  "The directory where this project's source code is located.")

(defcustom environ-dir (expand-file-name "~/")
  "Directory with env files."
  :group 'env
  :type 'directory)

(defcustom environ-pre-eval-hook nil
  "List of functions to run before subshell evaluation."
  :group 'env
  :type 'hook)

(defcustom environ-post-eval-hook nil
  "List of functions to run after subshell evaluation."
  :group 'env
  :type 'hook)

;;; Files

;;;###autoload
(defun environ-set-file (file-path)
  "Set environment variables defined in the file at FILE-PATH.

When used interactively, prompts for the file to load. The prompt
begins in `environ-dir'. When used from elisp, FILE-PATH can
either be absolute or relative to `default-directory'."
  (interactive (list (read-file-name "ENV file: " environ-dir)))
  (environ-set-str (f-read-text file-path)))

;;;###autoload
(defun environ-unset-file (file-path)
  "Unset the environment variables defined in FILE-PATH.

See the documentation for `environ-set-file'."
  (interactive (list (read-file-name "ENV file: " environ-dir)))
  (environ-unset-str (f-read-text file-path)))

;;; Strings

(defun environ-set-str (str)
  "Set environment variables defined by the given string STR.

Parse STR like an env file. STR is split into newline-delimited
lines, where each line is a key/value pair."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (environ--lines-to-pairs lines)))
    (environ-set-pairs pairs)))

(defun environ-unset-str (str)
  "Unset environment variables defined by string STR.

Parse STR like an env file. STR is split into newline-delimited
pairs, where each line is a key/value pair. The value of each
pair is discarded, as the environment variable will be unset
regardless of its value."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (environ--lines-to-pairs lines)))
    (environ-unset-pairs pairs)))

;;; Pairs

(defun environ-get-pairs ()
  "Return all current environment variables as a list of pairs."
  (environ--lines-to-pairs process-environment))

(defun environ-set-pairs (pairs)
  "Set environment variables defined by the given PAIRS.

PAIRS is a list of pairs, where each pair is an environment
variable name and value."
  (-> pairs
      (environ--eval-pairs)
      (-each #'environ--set-pair)))

(defun environ-unset-pairs (pairs)
  "Unset the environment variables defined in the given PAIRS.

PAIRS is a list of pairs, where each pair is an environment
variable name and value. The value in each pair doesn't matter;
each environment variable will be unset regardless of its value."
  (environ-unset-names (-map 'car pairs)))

;;; Names

(defun environ-get-names ()
  "Return a list of all current environment variable names."
  (--map
   (car (s-split "=" it))
   process-environment))

(defun environ-unset-names (names)
  "Unset environment variables with the given NAMES.

NAMES is a list of environment variable names which may or may
not be currently set. This function removes each name from
`process-environment' if it is set."
  (-each names #'environ-unset-name))

;;;###autoload
(defun environ-unset-name (name)
  "Unset the environment variable NAME.

Unset the given environment variable by removing it from
`process-environment' if it is there. Note that calling `setenv'
with a prefix argument can unset a variable by setting its value
to nil, but the variable remains in `process-environment'. This
function completely removes the variable from
`process-environment'.

Neither Emacs nor bash directly support non-ASCII characters as
environment variables (see [The Open Group][1]), but Emacs can
fake it by using escaped sequences of unicode code points.

[1]: https://pubs.opengroup.org/onlinepubs/9699919799/"
  (interactive (list (completing-read "" (environ-get-names))))
  (let* ((encoded-name (if (multibyte-string-p name)
                           (encode-coding-string name locale-coding-system t)
                         name))
         (index (-elem-index encoded-name (environ-get-names))))
    (if index
        (setq process-environment
              (-remove-at index process-environment))
      process-environment)))

;;; Pre-eval filters

;; Some pre-made post-eval filters. A pre-eval filter is a function that takes a
;; list of pairs and returns a list of pairs.

;; TODO!

;;; Post-eval filters

;; Some pre-made post-eval filters. A post-eval filter is a function that takes
;; a list of pairs and returns a list of pairs.

(defun environ-remove-sh-vars (pairs)
  "Remove some from PAIRS.

The sh shell initializes these environment varibales.

This is the default post-eval filter."
  (let ((ignored-environ-vars '("DISPLAY"
                            "PWD"
                            "SHLVL"
                            "_")))
    (-filter
     (lambda (pair) (not (member (car pair) ignored-environ-vars)))
     pairs)))

;;; Private functions

(defun environ--set-pair (pair)
  "Set an environment variable PAIR."
  (let ((name (car pair))
        (val (car (cdr pair))))
    (setenv name val)))

;;;; Conversion functions

(defun environ--pairs-to-script (pairs)
  "Turn PAIRS into a sh script."
  (->> pairs
       (--map (s-join "=" it))
       (environ--lines-to-script)))

(defun environ--lines-to-script (lines)
  "Turn LINES into a sh script."
  (->> lines
      (--map (s-prepend "export " it))
      (s-join "\n")))

(defun environ--lines-to-pairs (lines)
  "Return a list of pairs of LINES."
  (--map (s-split "=" it) lines))

;;;; Eval functions

(defun environ--eval-pairs (pairs)
  "Eval PAIRS.

- Capture the current process environment
- Run pre-eval hooks
- Evaluate given pairs in a subshell
- Run post-eval hooks

The result is diffed against the captured process environment.

Returns the list of pairs of environment variables that should be
set in the new environment."
  (let* (;; Capture the current environment
         (old-pairs (environ--lines-to-pairs process-environment))

         ;; TODO: Run the pre-eval hooks

         ;; Evaluate given pairs in a subshell
         (stdout (environ--eval-script
                  (environ--pairs-to-script pairs)))

         ;; Convert the result into pairs
         (out-pairs (-> stdout
                        s-trim
                        s-lines
                        environ--lines-to-pairs))

         ;; TODO: Run the post-eval hooks. Right now this is just the
         ;; remove-sh-vars function.
         (new-pairs (environ-remove-sh-vars out-pairs)))

    ;; And return the difference!
    (-difference new-pairs old-pairs)))

(defun environ--eval-script (script)
  "Start a subprocess, execute SCRIPT, and return the resulting env.

SCRIPT can be any sh script. This function appends the `env`
command to the end of the script, and then returns stdout."
  (with-temp-buffer
    (let ((environ-script (s-append "\nenv" script)))
      (call-process "sh"
                    nil t nil
                    shell-command-switch
                    environ-script)
      (buffer-string))))

(provide 'environ)
;;; environ.el ends here
