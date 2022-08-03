# Srcweave Literate Mode[]{#c0}

By: [**Eric Ihli**](https://owoga.com)

View the final code and other resources in the [GitHub
repo](https://github.com/eihli/lit-mode).

I'm going to follow the
[ModeTutorial](https://www.emacswiki.org/emacs/ModeTutorial) from the
Emacs Wiki.

1.  [The entry function](#s0:0)
2.  [Preamble](#s0:1)

> **Note:** This tutorial is a [literate
> program](https://en.wikipedia.org/wiki/Literate_programming). This
> means you are reading the source code right now! Each piece of code
> will be shown and explained thoroughly, so you can be sure nothing is
> left out. The final code was created by
> ["tangling"](https://github.com/justinmeiners/srcweave) the blocks of
> code together.

First, we define some variables that all modes should define.
'wpdl-mode-hook' allows the user to run their own code when your mode is
run.

::: code-block
[ ***[Hooks](#hooks-block-1){#hooks-block-1}***]{.block-header}

``` prettyprint
(defvar lit-mode-mode-hook nil)
```

[Used by [1](#-lit-mode.el-block-13 "/lit-mode.el")]{.small}
:::

Now we create a keymap. This map, here called 'lit-mode-map', allows
both you and users to define their own keymaps. The keymap is
immediately set to a default keymap. Then, using 'define-key', we insert
an example keybinding into the keymap, which maps the
'newline-and-indent' function to Control-j (which is actually the
default binding for this function, but is included anyway as an
example). Of course, you may define as many keybindings as you wish.

If your keymap will have very few entries, then you may want to consider
'make-sparse-keymap' rather than 'make-keymap'.

::: code-block
[ ***[Keymap](#keymap-block-3){#keymap-block-3}***]{.block-header}

``` prettyprint
(defvar lit-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Srcweave Literate major mode.")
```

[Used by [1](#-lit-mode.el-block-13 "/lit-mode.el")]{.small}
:::

Here, we append a definition to 'auto-mode-alist'. This tells emacs that
when a buffer with a name ending with .wpd is opened, then wpdl-mode
should be started in that buffer. Some modes leave this step to the
user.

::: code-block
[ ***[Autoload](#autoload-block-5){#autoload-block-5}***]{.block-header}

``` prettyprint
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lit\\'" . lit-mode))
```

[Used by [1](#-lit-mode.el-block-13 "/lit-mode.el")]{.small}
:::

Now we have defined our minimal set of keywords for emacs to highlight.
A 'font-lock-keyword' variable is a list of keywords to highlight. There
are many ways to specify this list. I have used the form (matcher .
facename). With this form, I have specified a pattern to match, and then
a face name to use for the actual highlighting.

There are two elements to my list: the first element matches WPDL
language keywords, and the second element matches WPDL identifier names
(variable names). I have selected the appropriate font-lock face names
for each type of keyword ('font-lock-builtin-face' and
'font-lock-variable-name-face', respectively).

For my keyword list, I've selected those WPDL keywords which would
benefit most from being highlighted: keywords that delimit blocks of
information. One may notice that the regexp used to specify these
keywords is optimized. I did not have to do this by hand. Emacs provides
the 'regexp-opt' function to save you from the tedious work of creating
complicated regexps. 'regexp-opt' takes a list of strings and an
additional optional argument. This optional argument controls whether or
not we want to wrap the entire regexp in parens. In our case, we do. For
example, the following expression:

::: code-block
[ ***[Syntax
regex](#syntax-regex-block-7){#syntax-regex-block-7}***]{.block-header}

``` prettyprint
(regexp-opt '("---")' t)
"\\(---\\)"
```
:::

::: code-block
[ ***[Syntax
highlighting](#syntax-highlighting-block-9){#syntax-highlighting-block-9}***]{.block-header}

``` prettyprint
(defconst lit-font-lock-code-blocks
  (list
   '("\\(---\\)". font-lock-builtin-face)
   '("---.*\\('\\w+'\\)" . font-lock-variable-name-face)
   '("@{\\(\w+\\)}" . font-lock-variable-name-face))
  "Minimal highlighting expressions for lit mode.")
```

[Used by [1](#-lit-mode.el-block-13 "/lit-mode.el")]{.small}
:::

## 1. The entry function[]{#s0:0}

Finally, we will create the function that will be called by Emacs when
the mode is started.

::: code-block
[ ***[Entry
function](#entry-function-block-11){#entry-function-block-11}***]{.block-header}

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

[Used by [1](#-lit-mode.el-block-13 "/lit-mode.el")]{.small}
:::

::: code-block
[
***[/lit-mode.el](#-lit-mode.el-block-13){#-lit-mode.el-block-13}***]{.block-header}

``` prettyprint
@{Preamble}
@{Hooks}
@{Keymap}
@{Autoload}
@{Syntax highlighting}
@{Entry function}

(provide 'lit-mode)
@{Postamble}
```
:::

## 2. Preamble[]{#s0:1}

::: code-block
[
***[Preamble](#preamble-block-15){#preamble-block-15}***]{.block-header}

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
```

[Used by [1](#-lit-mode.el-block-13 "/lit-mode.el")]{.small}
:::

### Postamble

::: code-block
[
***[Postamble](#postamble-block-17){#postamble-block-17}***]{.block-header}

``` prettyprint
;;; lit-mode.el ends here
```

[Used by [1](#-lit-mode.el-block-13 "/lit-mode.el")]{.small}
:::
