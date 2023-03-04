#!emacs --script
(require 'org)
;; (require 'org-babel)

(let ((init-file "~/.config/emacs/litfig.org"))
  ;; (print init-file)
  (org-babel-tangle-file init-file)
  )
