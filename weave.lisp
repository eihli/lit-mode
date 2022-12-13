; How to load a local ASDF?
; https://stackoverflow.com/a/65303394/425756

; This is just for development debugging
(asdf:defsystem "litweave"
    :version "0.0.1"
    :author "Eric Ihli"
    :license "GPL 2"
    :depends-on (:srcweave))

(defpackage :litweave
  (:use :cl))

(in-package :litweave)

(ql:quickload "srcweave")
(let ((sb-ext:*posix-argv* '("--tangle" "/docs/src/" "index.lit")))
  (srcweave::start-command))
