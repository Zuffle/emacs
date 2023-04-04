#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(require 'org)

(let ((config-file (concat
		    (file-name-directory (buffer-file-name))
		    "listfig.org"
		    ))
      )
  (princ (format "Tangling file %s.\n" config-file))
  (org-babel-tangle-file config-file)
  )
