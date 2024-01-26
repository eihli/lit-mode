# Srcweave Literate Mode<span id="c0"></span>

By: [**Eric Ihli**](https://owoga.com)

View the final code and other resources in the [GitHub
repo](https://github.com/eihli/lit-mode).

This is an Emacs major-mode for literate programming in
[srcweave](https://github.com/justinmeiners/srcweave), *written as a
literate program using srcweave*.

This will be a bit of a rehash of
[ModeTutorial](https://www.emacswiki.org/emacs/ModeTutorial) from the
Emacs Wiki. Thanks to [Scott Andrew
Borton](https://www.emacswiki.org/emacs/ScottAndrewBorton) for that
tutorial.

1.  [Srcweave Literate Mode](#c0)
2.  [Hooks](#c1)
3.  [Keymap](#c2)
4.  [Autoload](#c3)
5.  [Syntax highlighting](#c4)
    1.  [Single " turning on highlighting](#s4:0)
6.  [Indentation](#c5)
7.  [Editing a code block](#c6)
8.  [The entry function](#c7)
9.  [Preamble](#c8)

> **Note:** This tutorial is a [literate
> program](https://en.wikipedia.org/wiki/Literate_programming). This
> means you are reading the source code right now\! Each piece of code
> will be shown and explained thoroughly, so you can be sure nothing is
> left out. The final code was created by
> ["tangling"](https://github.com/justinmeiners/srcweave) the blocks of
> code together.

# Hooks<span id="c1"></span>

First, we define some variables that all modes should define.
‘lit-mode-hook’ allows the user to run their own code when your mode
is run.

<div class="code-block">

<span class="block-header"> ***[Hooks](#hooks-block-1)***</span>

``` prettyprint
(defvar lit-mode-mode-hook nil)
```

<span class="small">Used by
[1](#-lit-mode.el-block-23 "/lit-mode.el")</span>

</div>

# Keymap<span id="c2"></span>

Now we create a keymap. This map, here called ‘lit-mode-map’, allows
both you and users to define their own keymaps. The keymap is
immediately set to a default keymap. Then, using ‘define-key’, we insert
an example keybinding into the keymap, which maps the
‘newline-and-indent’ function to Control-j (which is actually the
default binding for this function, but is included anyway as an
example). Of course, you may define as many keybindings as you wish. If
your keymap will have very few entries, then you may want to consider
‘make-sparse-keymap’ rather than ‘make-keymap’.

<div class="code-block">

<span class="block-header"> ***[Keymap](#keymap-block-3)***</span>

``` prettyprint
(defvar lit-mode-map
  (let ((map (make-keymap)))
    @{Keybindings}
    map)
  "Keymap for Srcweave Literate major mode.")
```

<span class="small">Used by
[1](#-lit-mode.el-block-23 "/lit-mode.el")</span>

</div>

<div class="code-block">

<span class="block-header">
***[Keybindings](#keybindings-block-5)***</span>

``` prettyprint
(define-key map (kbd "C-j") 'newline-and-indent)
```

<span class="small">Used by [1](#keymap-block-3 "Keymap")</span>

</div>

# Autoload<span id="c3"></span>

Here, we append a definition to ‘auto-mode-alist’. This tells emacs that
when a buffer with a name ending with .wpd is opened, then wpdl-mode
should be started in that buffer. Some modes leave this step to the
user.

<div class="code-block">

<span class="block-header"> ***[Autoload](#autoload-block-7)***</span>

``` prettyprint
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lit\\'" . lit-mode))
```

<span class="small">Used by
[1](#-lit-mode.el-block-23 "/lit-mode.el")</span>

</div>

# Syntax highlighting<span id="c4"></span>

Next let's define a minimal set of keywords for emacs to highlight. A
‘font-lock-keyword’ variable is a list of keywords to highlight. There
are many ways to specify this list. I'll use the form (matcher .
facename). With this form, I'll specify a pattern to match, and then a
face name to use for the actual highlighting.

<div class="code-block">

<span class="block-header"> ***[Syntax
highlighting](#syntax-highlighting-block-9)***</span>

``` prettyprint
(defconst lit-font-lock-code-blocks
  (list
   '("^\\(---\\)". font-lock-doc-markup-face)
   '("^---[\t ]*\\([^\-\n/]+\\)" . (1 font-lock-constant-face))
   '("@{\\([^}]+\\)}" . (1 font-lock-constant-face)))
  "Minimal highlighting expressions for lit mode.")
```

<span class="small">Used by
[1](#-lit-mode.el-block-23 "/lit-mode.el")</span>

</div>

There are three elements to my list: the first element matches srcweave
code block syntax, \`---\`. The second element matches the declaration
of the reference name (the name used to include the contents of one
source code block into another). And the third matches the \_use\_ of
the reference name (e.g. \`@{SomeCodeBlock}\`). I'm going to borrow some
existing faces with names close-enough to what I want. For the open and
close of a code block, I'm going to use \`font-lock-doc-markup-face\`.
For the code block reference names, I'll use
\`font-lock-constant-face\`. For my keyword list, I’ve selected those
WPDL keywords which would benefit most from being highlighted: keywords
that delimit blocks of information. One may notice that the regexp used
to specify these keywords is optimized. I did not have to do this by
hand. Emacs provides the ‘regexp-opt’ function to save you from the
tedious work of creating complicated regexps. ‘regexp-opt’ takes a list
of strings and an additional optional argument. This optional argument
controls whether or not we want to wrap the entire regexp in parens. In
our case, we do. For example, the following expression:

## 1\. Single " turning on highlighting<span id="s4:0"></span>

This is best demonstrated with an image. It's hard to know when a \`"\`
should be the start of a string that we want to highlight and when it's
just a lone character that shouldn't turn on highlighting. I'm finding
it nicer to just have it turned off. It wasn't easy to find. (defvar
font-lock-keywords-only nil "Non-nil means Font Lock should not fontify
comments or strings. This is normally set via \`font-lock-defaults'.")
Although this is technically a syntax highlighting topic, I'll put this
in lit-mode-hooks so it will run when the mode is activated.

<div class="code-block">

<span class="block-header">
***[lit-mode-hooks](#lit-mode-hooks-block-11)***</span>

``` prettyprint
(setq font-lock-keywords-only t)
```

<span class="small">Used by
[1](#indentation-block-13 "Indentation")</span>

</div>

# Indentation<span id="c5"></span>

Indentation is very fundamental. I'm going to default to tabs for
indentation.

<div class="code-block">

<span class="block-header">
***[Indentation](#indentation-block-13)***</span>

``` prettyprint
(add-hook 'lit-mode-hook
          (lambda ()
            @{lit-mode-hooks}
            (setq indent-tabs-mode t)
            (setq indent-tabs-function #'tab-to-tab-stop)
            (setq indent-line-function #'indent-to-left-margin)
            (setq display-line-numbers-mode t)
            (setq tab-width 4)))
```

<span class="small">Used by
[1](#-lit-mode.el-block-23 "/lit-mode.el")</span>

</div>

# Editing a code block<span id="c6"></span>

I want to be able to take advantage of major modes for whatever language
I'm working with. Language-specific major modes are especially nice for
things like formatting and alignment. This gives us syntax highlighting
but gives us errors from LSP.
https://emacs-lsp.github.io/lsp-mode/manual-language-docs/lsp-org/

<div class="code-block">

<span class="block-header"> ***[Editing a code
block](#editing-a-code-block-block-15)***</span>

``` prettyprint
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
```

<span class="small">Used by
[1](#-lit-mode.el-block-23 "/lit-mode.el")</span>

</div>

Narrowing a buffer makes it \_look\_ like you're only operating on a
small section of a buffer. But any buffer configuration, like the mode
line, operate on the entire buffer even if you can't see the entire
buffer in your window. This becomes a problem if you narrow to a region
of C code and then turn on c-mode. The mode will complain because the
buffer isn't valid C. Emacs offers a \`clone-indirect\` that creates a
copy of a buffer and changes made to one buffer get reflected in the
other. But in hindsight. That doesn't work either.
https://github.com/aaronbieber/fence-edit.el/blob/master/fence-edit.el

<div class="code-block">

<span class="block-header"> ***[Editing a code block
2](#editing-a-code-block-2-block-17)***</span>

``` prettyprint
(defvar lit-mode-code-block-mode 'c-mode)

(defmacro with-indirect-buffer (beg end &rest body)
  (let ((saved-mode major-mode))
    `(with-current-buffer (clone-indirect-buffer nil nil)
        (narrow-to-region beg end)
        (funcall lit-mode-code-block-mode)
        (unwind-protect
            ,body
        (kill-buffer (current-buffer))))))

(defun lit-mode-points-surrounding-code-block ()
 (save-excursion
    (let ((beg)
          (end))
        (search-backward-regexp "^--- ")
        (next-line)
        (beginning-of-line)
        (setq beg (point))
        (search-forward-regexp "^---$")
        (previous-line)
        (end-of-line)
        (setq end (point))
        (cl-values beg end))))

(defun lit-mode-edit-region-in-mode ()
  (interactive "@r")
  (let ((new-buffer (clone-indirect-buffer nil t)))
    (cl-multiple-value-bind (beg end) (lit-mode-points-surrounding-code-block)
        (narrow-to-region beg end)
        (funcall lit-mode-code-block-mode))))
```

</div>

<div class="code-block">

<span class="block-header"> ***[Keybindings](#keybindings-block-19)***
[+=](#keybindings-block-5)</span>

``` prettyprint
(define-key map (kbd "C-;") #'lit-mode-narrow-to-code-block-for-editing)
```

</div>

# The entry function<span id="c7"></span>

Finally, we will create the function that will be called by Emacs when
the mode is started.

<div class="code-block">

<span class="block-header"> ***[Entry
function](#entry-function-block-21)***</span>

``` prettyprint
(defun lit-mode ()
  "Major mode for editing srcweave literate files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map lit-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(lit-font-lock-code-blocks))
  (setq major-mode 'lit-mode)
  (setq mode-name "Lit")
  (run-hooks 'lit-mode-hook))
```

<span class="small">Used by
[1](#-lit-mode.el-block-23 "/lit-mode.el")</span>

</div>

<div class="code-block">

<span class="block-header">
***[/lit-mode.el](#-lit-mode.el-block-23)***</span>

``` prettyprint
@{Preamble}
@{Hooks}
@{Keymap}
@{Autoload}
@{Syntax highlighting}
@{Indentation}
@{Editing a code block}
@{Entry function}

(provide 'lit-mode)
@{Postamble}
```

</div>

# Preamble<span id="c8"></span>

`advice-add` requires a minimum Emacs version of 24.4, so we set that in
the preamble.

<div class="code-block">

<span class="block-header"> ***[Preamble](#preamble-block-25)***</span>

``` prettyprint
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
```

<span class="small">Used by
[1](#-lit-mode.el-block-23 "/lit-mode.el")</span>

</div>

### Postamble

<div class="code-block">

<span class="block-header">
***[Postamble](#postamble-block-27)***</span>

``` prettyprint
;;; lit-mode.el ends here
```

<span class="small">Used by
[1](#-lit-mode.el-block-23 "/lit-mode.el")</span>

</div>
