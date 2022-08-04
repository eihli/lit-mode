# Srcweave Literate Mode<span id="c0"></span>

By: [**Eric Ihli**](https://owoga.com)

View the final code and other resources in the [GitHub
repo](https://github.com/eihli/lit-mode).

I’m going to follow the
[ModeTutorial](https://www.emacswiki.org/emacs/ModeTutorial) from the
Emacs Wiki to create mode for
[srcweave](https://github.com/justinmeiners/srcweave).

1.  [The entry function](#s0:0)
2.  [Preamble](#s0:1)

> **Note:** This tutorial is a [literate
> program](https://en.wikipedia.org/wiki/Literate_programming). This
> means you are reading the source code right now! Each piece of code
> will be shown and explained thoroughly, so you can be sure nothing is
> left out. The final code was created by
> [“tangling”](https://github.com/justinmeiners/srcweave) the blocks of
> code together.

First, we define some variables that all modes should define.
‘lit-mode-hook’ allows the user to run their own code when your mode is
run.

<div class="code-block">

<span class="block-header">
***<a href="#hooks-block-1" id="hooks-block-1">Hooks</a>***</span>

``` prettyprint
(defvar lit-mode-mode-hook nil)
```

<span class="small">Used by
[1](#-lit-mode.el-block-11 "/lit-mode.el")</span>

</div>

Now we create a keymap. This map, here called ‘lit-mode-map’, allows
both you and users to define their own keymaps. The keymap is
immediately set to a default keymap. Then, using ‘define-key’, we insert
an example keybinding into the keymap, which maps the
‘newline-and-indent’ function to Control-j (which is actually the
default binding for this function, but is included anyway as an
example). Of course, you may define as many keybindings as you wish.

If your keymap will have very few entries, then you may want to consider
‘make-sparse-keymap’ rather than ‘make-keymap’.

<div class="code-block">

<span class="block-header">
***<a href="#keymap-block-3" id="keymap-block-3">Keymap</a>***</span>

``` prettyprint
(defvar lit-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Srcweave Literate major mode.")
```

<span class="small">Used by
[1](#-lit-mode.el-block-11 "/lit-mode.el")</span>

</div>

Here, we append a definition to ‘auto-mode-alist’. This tells emacs that
when a buffer with a name ending with .wpd is opened, then wpdl-mode
should be started in that buffer. Some modes leave this step to the
user.

<div class="code-block">

<span class="block-header">
***<a href="#autoload-block-5" id="autoload-block-5">Autoload</a>***</span>

``` prettyprint
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lit\\'" . lit-mode))
```

<span class="small">Used by
[1](#-lit-mode.el-block-11 "/lit-mode.el")</span>

</div>

Next let’s define a minimal set of keywords for emacs to highlight. A
‘font-lock-keyword’ variable is a list of keywords to highlight. There
are many ways to specify this list. I’ll use the form (matcher .
facename). With this form, I’ll specify a pattern to match, and then a
face name to use for the actual highlighting.

<div class="code-block">

<span class="block-header"> ***<a href="#syntax-highlighting-block-7"
id="syntax-highlighting-block-7">Syntax highlighting</a>***</span>

``` prettyprint
(defconst lit-font-lock-code-blocks
  (list
   '("^\\(---\\)". font-lock-doc-markup-face)
   '("^---[\t ]*\\([^\-\n/]+\\)" . (1 font-lock-constant-face))
   '("@{\\([^}]+\\)}" . (1 font-lock-constant-face)))
  "Minimal highlighting expressions for lit mode.")
```

<span class="small">Used by
[1](#-lit-mode.el-block-11 "/lit-mode.el")</span>

</div>

There are three elements to my list: the first element matches srcweave
code block syntax, `---`. The second element matches the declaration of
the reference name (the name used to include the contents of one source
code block into another). And the third matches the *use* of the
reference name (e.g. `@{SomeCodeBlock}`).

I’m going to borrow some existing faces with names close-enough to what
I want. For the open and close of a code block, I’m going to use
`font-lock-doc-markup-face`. For the code block reference names, I’ll
use `font-lock-constant-face`.

For my keyword list, I’ve selected those WPDL keywords which would
benefit most from being highlighted: keywords that delimit blocks of
information. One may notice that the regexp used to specify these
keywords is optimized. I did not have to do this by hand. Emacs provides
the ‘regexp-opt’ function to save you from the tedious work of creating
complicated regexps. ‘regexp-opt’ takes a list of strings and an
additional optional argument. This optional argument controls whether or
not we want to wrap the entire regexp in parens. In our case, we do. For
example, the following expression:

## 1. The entry function<span id="s0:0"></span>

Finally, we will create the function that will be called by Emacs when
the mode is started.

<div class="code-block">

<span class="block-header">
***<a href="#entry-function-block-9" id="entry-function-block-9">Entry
function</a>***</span>

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
[1](#-lit-mode.el-block-11 "/lit-mode.el")</span>

</div>

<div class="code-block">

<span class="block-header"> ***<a href="#-lit-mode.el-block-11"
id="-lit-mode.el-block-11">/lit-mode.el</a>***</span>

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

</div>

## 2. Preamble<span id="s0:1"></span>

<div class="code-block">

<span class="block-header">
***<a href="#preamble-block-13" id="preamble-block-13">Preamble</a>***</span>

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

<span class="small">Used by
[1](#-lit-mode.el-block-11 "/lit-mode.el")</span>

</div>

### Postamble

<div class="code-block">

<span class="block-header">
***<a href="#postamble-block-15" id="postamble-block-15">Postamble</a>***</span>

``` prettyprint
;;; lit-mode.el ends here
```

<span class="small">Used by
[1](#-lit-mode.el-block-11 "/lit-mode.el")</span>

</div>
