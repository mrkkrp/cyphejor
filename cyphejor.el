;;; cyphejor.el --- Shorten major mode names using user-defined rules -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/cyphejor
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: mode-line major-mode
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows to shorten major mode names using user-defined rules.

;;; Code:

(require 'cl-lib)

(defgroup cyphejor nil
  "Shorten major mode names using user-defined rules"
  :group  'convenience
  :tag    "Cyphejor"
  :prefix "cyphejor-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/cyphejor"))

(defcustom cyphejor-rules nil
  "TODO")

(defun cyphejor--cypher (old-name rules)
  "Convert OLD-NAME into its shorter form following RULES."
  nil) ;; TODO

(defun cyphejor--hook ()
  "Set `mode-name' according of symbol name in `major-mode'.

This uses `cyphejor--cypher' and `cyphejor-rules' to generate new
mode name."
  (setq mode-name
        (cyphejor--cypher
         (symbol-name major-mode)
         cyphejor-rules)))

;;;###autoload
(define-minor-mode cyphejor-mode
  "Toggle `cyphejor-mode' minor mode.

With a prefix argument ARG, enable `cyphejor-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.

This global minor mode shortens names of major modes
automatically following user-defined rules in
`cyphejor-rules'. See description of the variable for more
information."
  nil "" nil
  :global t
  (funcall (if cyphejor-mode #'add-hook #'remove-hook)
           'after-change-major-mode-hook
           #'cyphejor--hook))

(provide 'cyphejor)

;;; cyphejor.el ends here
