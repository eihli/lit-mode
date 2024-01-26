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
;; Homepage: https://github.com/eihli/lit-mode
;; Package-Requires: ((emacs "24.4"))
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
    (define-key map (kbd "C-j") 'newline-and-indent)
    (define-key map (kbd "C-;") #'lit-mode-narrow-to-code-block-for-editing)
    map)
  "Keymap for Srcweave Literate major mode.")
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lit\\'" . lit-mode))
(defconst lit-font-lock-code-blocks
  (list
   '("^\\(---\\)". font-lock-doc-markup-face)
   '("^---[\t ]*\\([^\-\n/]+\\)" . (1 font-lock-constant-face))
   '("@{\\([^}]+\\)}" . (1 font-lock-constant-face)))
  "Minimal highlighting expressions for lit mode.")
(add-hook 'lit-mode-hook
          (lambda ()
            (setq font-lock-keywords-only t)
            (setq indent-tabs-mode t)
            (setq indent-tabs-function #'tab-to-tab-stop)
            (setq indent-line-function #'indent-to-left-margin)
            (setq display-line-numbers-mode t)
            (setq tab-width 4)))
(defvar lit-mode-code-block-mode 'emacs-lisp-mode)
(defvar lit-mode-saved-mode nil)

(defun lit-mode-widen-advice ()
  (funcall (symbol-function lit-mode-saved-mode))
  (setq lit-mode-saved-mode nil)
  (advice-remove 'widen #'lit-mode-widen-advice))

(defun lit-mode-narrow-to-code-block-for-editing ()
  (interactive)
  (let ((beg)
        (end))
    (setq lit-mode-saved-mode major-mode)
    (advice-add 'widen :after #'lit-mode-widen-advice)
    (save-excursion
      (search-backward-regexp "^--- ")
      (next-line)
      (beginning-of-line)
      (setq beg (point))
      (search-forward-regexp "^---$")
      (previous-line)
      (end-of-line)
      (setq end (point))
      (narrow-to-region beg end)
      (funcall lit-mode-code-block-mode))))
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
