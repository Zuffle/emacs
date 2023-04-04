#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(require 'org)

(let ((config-file (concat default-directory "litfig.org"))
      )
  (princ (format "Tangling snippets from %s.\n" config-file))
  (org-babel-tangle-file config-file)
  )
