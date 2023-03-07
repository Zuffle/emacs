#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(require 'org)
;; (require 'org-babel)

(let ((init-file "~/.config/emacs/litfig.org"))
  ;; (print init-file)
  (org-babel-tangle-file init-file)
  )
