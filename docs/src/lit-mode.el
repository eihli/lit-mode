;;; lit-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Eric Ihli
;;
;; Author: Eric Ihli <eihli@owoga.com>
;; Maintainer: Eric Ihli <eihli@owoga.com>
;; Created: August 02, 2022
;; Modified: August 02, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/eihli/foo
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(defvar lit-mode-mode-hook nil)
(defvar lit-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Srcweave Literate major mode.")
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lit\\'" . lit-mode))
(defconst lit-font-lock-code-blocks
  (list
   '("\\(---\\)". font-lock-builtin-face)
   '("---[\t ]*\\([^\-\n/]+\\)" . (1 font-lock-variable-name-face))
   '("@{\\([^}]+\\)}" . (1 font-lock-variable-name-face)))
  "Minimal highlighting expressions for lit mode.")
(defun lit-mode ()
  "Major mode for editing srcweave literate files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map lit-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(lit-font-lock-code-blocks))
  (setq major-mode 'lit-mode)
  (setq mode-name "Lit")
  (run-hooks 'lit-mode-hook))

(provide 'lit-mode)
;;; lit-mode.el ends here
