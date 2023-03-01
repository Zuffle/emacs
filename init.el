(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package general
  :config
  (general-evil-setup t)
  )

(general-setq inhibit-startup-message t)

(general-setq source-directory "c:/Program Files/Emacs/emacs-27.2")

(general-setq
 safe-local-variable-values
 '((eval add-hook 'after-save-hook
	 (lambda nil
	   (if
	       (y-or-n-p "Tangle?")
	       (org-babel-tangle)))
	 nil t)))

(defun bookmark-to-abbrevs ()
   "Create abbrevs based on `bookmark-alist'."
   (dolist (bookmark bookmark-alist)
   (let* ((name (car bookmark))
          (file (bookmark-get-filename name)))
     (define-abbrev global-abbrev-table name file))))

(defun my-move-key (keymap-from keymap-to key)
     "Moves key binding from one keymap to another, deleting from the old location. "
     (define-key keymap-to key (lookup-key keymap-from key))
     (define-key keymap-from key nil))

(defun disable-line-numbers-mode ()
    "A function to add to mode hooks to prevent line numbers"
    (display-line-numbers-mode 0))

;; evil emacs state mode hook
(defun jep/evil-emacs-state-hook ()
  (dolist (mode '(eshell-mode
		  HM
		  howm-menu-mode
		  ))
    (add-to-list 'evil-emacs-state-modes mode)))

; Disable line numbers for modes in list
(dolist (mode '(term-mode-hook
		shell-mode-hook
		ehsell-mode-hook))
  (add-hook mode 'disable-line-numbers-mode))

 ;; (defun dw/switch-project-action ()
 ;;  "Switch to a workspace with the project name and start `magit-status'."
 ;;  ;; TODO: Switch to EXWM workspace 1?
 ;;  (persp-switch (projectile-project-name))
 ;;  (magit-status))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (general-setq evil-auto-indent nil)
  )


(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window.
If `spacemacs-layouts-restrict-spc-tab' is `t' then this only switches between
the current layouts buffers."
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (if (bound-and-true-p spacemacs-layouts-restrict-spc-tab)
          (let ((buffer-list (persp-buffer-list))
                (my-buffer (window-buffer window)))
            ;; find buffer of the same persp in window
            (seq-find (lambda (it) ;; predicate
                        (and (not (eq (car it) my-buffer))
                             (member (car it) buffer-list)))
                      (window-prev-buffers)
                      ;; default if found none
                      (list nil nil nil)))
        (or (cl-find (window-buffer window) (window-prev-buffers)
                     :key #'car :test-not #'eq)
            (list (other-buffer) nil nil)))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))

(scroll-bar-mode -1)			;disable visible scroll bar
(tool-bar-mode -1)			; disable tool bar
(tooltip-mode -1)			; disable tooltips
(set-fringe-mode -1)			;give breathing room ?
(menu-bar-mode -1)			; disable menu bar

(general-setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode t)
(general-setq visible-bell t)			; change audio bells to visual

(set-face-attribute 'default nil :family "Fira Mono")

(load-theme 'tsdh-dark)

;; UTF-8 support

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)    
(general-setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(use-package counsel
  :init
  (ivy-mode 1))

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode)
  )

(use-package ivy-rich
  :general
  (
   "C-s" 'swiper
   "M-x" 'counsel-M-x
   "C-x b" 'counsel-ibuffer
   "C-x C-f" 'counsel-find-file
   "C-M-l" 'counsel-imenu
   )
  (:keymaps 'minibuffer-local-map
	    "C-r" '(counsel-minibuffer-history)
	    )
  (:keymaps 'ivy-minibuffer-map
	    "C-l" 'ivy-alt-done
	    "C-j" 'ivy-next-line
	    "C-k" 'ivy-previous-line
	    )
  (:keymaps 'ivy-switch-buffer-map
	    "C-k" 'ivy-previous-line
	    "C-l" 'ivy-done
	    "C-d" 'ivy-switch-buffer-kill
	    )
  (:keymaps 'ivy-reverse-i-search-map
	    "C-k" 'ivy-previous-line
	    "C-d" 'ivy-reverse-i-search-kill
	    )
  :init
  (ivy-rich-mode)
  )

(use-package ivy-hydra
  )

(use-package diminish
  )

(use-package doom-themes
  ;; :init (load-theme 'doom-wilmersdorf t)
)

(use-package evil
  :defer t
  :init
  (general-setq evil-want-integration t)
  (general-setq evil-want-keybinding nil)
  (general-setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (general-setq evil-undo-system 'undo-tree)
   (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
   ;; (my-move-key evil-motion-state-map evil-normal-state-map " ") 
   (general-unbind 'motion
     "SPC"
     )
   
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  )

(use-package evil-escape
  :config
  (evil-escape-mode 1)
  )

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  )

(use-package howm
  )

(use-package hydra
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

(use-package org
  :custom
  (org-src-preserve-indentation t)
  :config
  ;; efs 
  (general-setq org-ellipsis " ▾")

  ;; efs ep6 //
  ;; set which files should be used to populate agenda with tasks
  (general-setq org-agenda-files
	'("~/orgfiles/tasks.org"
	  "~/orgfiles/bdays.org")
	)
  (general-setq org-agenda-start-with-log-mode t)
  (general-setq org-log-done 'time)
  (general-setq org-log-into-drawer t)
  (general-setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (general-setq org-refile-targets
	'(("Archive.org" :maxlevel . 1)
	  ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (general-setq org-tag-alist
	'((:startgroup)
					; Put mutually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("planning" . ?p)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)))


  (general-setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
	   "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
	   (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
	   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
	   ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
	   :clock-in :clock-resume
	   :empty-lines 1)
	  ("jm" "Meeting" entry
	   (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
	   "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
	   :clock-in :clock-resume
	   :empty-lines 1)

	  ("w" "Workflows")
	  ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
	   "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

	  ("m" "Metrics Capture")
	  ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
	   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))
  ;; // efs ep6
;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))
  )


;; (efs/org-font-setup)

;; Configure custom agenda views
;; a bit overkill for me rn
;; (general-setq org-agenda-custom-commands
;;       '(("d" "Dashboard"
;; 	 ((agenda "" ((org-deadline-warning-days 7)))
;; 	  (todo "NEXT"
;; 		((org-agenda-overriding-header "Next Tasks")))
;; 	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

;; 	("n" "Next Tasks"
;; 	 ((todo "NEXT"
;; 		((org-agenda-overriding-header "Next Tasks")))))

;; 	("W" "Work Tasks" tags-todo "+work-email")

;; 	;; Low-effort next actions
;; 	("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
;; 	 ((org-agenda-overriding-header "Low Effort Tasks")
;; 	  (org-agenda-max-todos 20)
;; 	  (org-agenda-files org-agenda-files)))

;; 	("w" "Workflow Status"
;; 	 ((todo "WAIT"
;; 		((org-agenda-overriding-header "Waiting on External")
;; 		 (org-agenda-files org-agenda-files)))
;; 	  (todo "REVIEW"
;; 		((org-agenda-overriding-header "In Review")
;; 		 (org-agenda-files org-agenda-files)))
;; 	  (todo "PLAN"
;; 		((org-agenda-overriding-header "In Planning")
;; 		 (org-agenda-todo-list-sublevels nil)
;; 		 (org-agenda-files org-agenda-files)))
;; 	  (todo "BACKLOG"
;; 		((org-agenda-overriding-header "Project Backlog")
;; 		 (org-agenda-todo-list-sublevels nil)
;; 		 (org-agenda-files org-agenda-files)))
;; 	  (todo "READY"
;; 		((org-agenda-overriding-header "Ready for Work")
;; 		 (org-agenda-files org-agenda-files)))
;; 	  (todo "ACTIVE"
;; 		((org-agenda-overriding-header "Active Projects")
;; 		 (org-agenda-files org-agenda-files)))
;; 	  (todo "COMPLETED"
;; 		((org-agenda-overriding-header "Completed Projects")
;; 		 (org-agenda-files org-agenda-files)))
;; 	  (todo "CANC"
;; 		((org-agenda-overriding-header "Cancelled Projects")
;; 		 (org-agenda-files org-agenda-files)))))))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list #'("◉" "○" "✸" "✿"))
  ;; (org-superstar-leading-bullet ".")
  )

(use-package org-contrib
  :config
  (org-eldoc-load)
  )

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects/coding")
    (general-setq projectile-project-search-path '("~/projects/coding")))
  (general-setq projectile-switch-project-action #'dw/projectile-dired)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smex)

(use-package spaceline
  :config
  (spaceline-emacs-theme)
  )

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  )

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (general-setq which-key-idle-delay 0.3)
  ;; (general-setq which-key-separator " -> ")
  )

;; text scale hydra
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-decrease "out")
  ("k" text-scale-increase "in")
  ("0" (text-scale-set 0))
  ("q" nil "quit")
  )
  
;; buffer hydra
(defhydra hydra-buffer-nav (:timeout 4)
  "navigate buffers"
)

(general-create-definer jep/leader-keys
  :keymaps '(normal insert emacs visual)
  :prefix "SPC"
  :non-normal-prefix "C-SPC"
  "SPC" nil
  )

(jep/leader-keys
  "TAB" '(spacemacs/alternate-buffer :wk "last buffer")

  ;; prefix key setup
  "f" '(:ignore t :wk "files")
  "t" '(:ignore t :wk "toggles")
  "a" '(:ignore t :wk "applications")
  "w" '(:ignore t :wk "window")
  "h" '(:ignore t :wk "help")
  "b" '(:ignore t :wk "buffers")
  ";" '(:ignore t :wk "comment")
  "p" '(projectile-command-map :wk "projectile")
  "g" '(:ignore t :which-key "git")

  )

(jep/leader-keys
  :infix "b"
  "k" '(kill-current-buffer :wk)
  "b" '(counsel-switch-buffer :wk)
  "c" '(clean-buffer-list :wk)
  )

(jep/leader-keys
  :infix ";"
  ";" '(comment-line :wk)		
  )

;; FILES
(jep/leader-keys
  :infix "f"
  "f" '(counsel-find-file :wk)
  "s" '(save-buffer :wk)
  "r" '(counsel-recentf :wk)
  )

;; GIT
(jep/leader-keys
  :infix "g"
  "s"  'magit-status
  "d"  'magit-diff-unstaged
  "c"  'magit-branch-or-checkout
  "l"   '(:ignore t :which-key "log")
  "lc" 'magit-log-current
  "lf" 'magit-log-buffer-file
  "b"  'magit-branch
  "P"  'magit-push-current
  "p"  'magit-pull-branch
  "f"  'magit-fetch
  "F"  'magit-fetch-all
  "r"  'magit-rebase
  )

;; HELP
(jep/leader-keys
  :infix "h"
  "d" '(:ignore t :wk "describe")
  "dv" '(describe-variable :wk)
  "df" '(describe-function :wk)
  "dk" '(describe-key :wk)
  "dg" '(general-describe-keybindings :wk)
  "dm" '(describe-mode :wk)
  

  )

;; TOGGLES
(jep/leader-keys
  :infix "t"
  "t" '(counsel-load-theme :which-key)
  "s" '(hydra-text-scale/body :wk)
  )

;; SEARCH
(jep/leader-keys
  :infix "s"
  "s" '(swiper :wk)
  )

;; WINDOW
(jep/leader-keys
  :infix "w"
  "d" '(:wk)
  )

;; PROJECT
;; (jep/leader-keys
;;   :infix "p"
;;   "f"  'counsel-projectile-find-file
;;   "s"  'counsel-projectile-switch-project
;;   "F"  'counsel-projectile-rg
;;   ;; "pF"  'consult-ripgrep
;;   "p"  'counsel-projectile
;;   "c"  'projectile-compile-project
;;   "d"  'projectile-dired)
