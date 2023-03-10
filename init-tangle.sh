#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(require 'org)
;; (require 'org-babel)


(let ((config-file "~/.config/emacs-config/litfig.org")
      (init-file "~.config/emacs/init.el")
      (early-init "~.config/emacs/early-init.el")
      )
  ;; (print init-file)
  
  (org-babel-tangle-file init-file)
  )
