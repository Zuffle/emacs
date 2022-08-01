;; TODO change straight-use-package to use-package for syntactic sugar
;; that straight.el does not use, why reinvent the wheel right?
(setq inhibit-startup-message t)
(scroll-bar-mode -1)			;disable visible scroll bar
(tool-bar-mode -1)			; disable tool bar
(tooltip-mode -1)			; disable tooltips
(set-fringe-mode -1)			;give breathing room ?

(menu-bar-mode -1)			; disable menu bar
(setq visual-bell t)			; change audio bells to visual

(set-face-attribute 'default nil :family "Fira Code")

;; (load-theme 'wombat)

;;(setq straight-base-dir "~/.config/straight")

;; function to make abbrevs out of bookmarks
(defun bookmark-to-abbrevs ()
   "Create abbrevs based on `bookmark-alist'."
   (dolist (bookmark bookmark-alist)
   (let* ((name (car bookmark))
          (file (bookmark-get-filename name)))
     (define-abbrev global-abbrev-table name file))))


(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode t)
(defun disable-line-numbers-mode ()
    "A function to add to mode hooks to prevent line numbers"
    (display-line-numbers-mode 0))
  

; Disable line numbers for modes in list
(dolist (mode '(term-mode-hook
		shell-mode-hook
		ehsell-mode-hook))
  (add-hook mode 'disable-line-numbers-mode))

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
  ;; (general-evil-setup t)
  )

(use-package counsel
  :init
  (ivy-mode 1))

(use-package diminish
  )

(use-package spaceline
  :config
  (spaceline-emacs-theme)
  )

(use-package doom-themes
  :init (load-theme 'doom-wilmersdorf t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

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
	    "TAB" 'ivy-alt-done	
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

(general-setq source-directory "c:/Program Files/Emacs/emacs-27.2")

(use-package evil
  :defer t
  :init
  (general-setq evil-want-integration t)
  (general-setq evil-want-keybinding nil)
  (general-setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (general-setq evil-undo-system 'undo-redo)
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

(use-package howm
  )

(use-package hydra
  )

 ;; (defun dw/switch-project-action ()
 ;;  "Switch to a workspace with the project name and start `magit-status'."
 ;;  ;; TODO: Switch to EXWM workspace 1?
 ;;  (persp-switch (projectile-project-name))
 ;;  (magit-status))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects/coding")
    (setq projectile-project-search-path '("~/projects/coding")))
  (setq projectile-switch-project-action #'dw/projectile-dired)
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )


(use-package counsel-projectile
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode)
  )

 

;;;;;;;;;;;;;;;;;;
;; Hydra Macros ;;
;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings		  	     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-def
  "C-g" 'evil-escape
  )

(general-create-definer jep/leader-keys
  :keymaps '(normal insert emacs visual)
  :prefix "SPC"
  :non-normal-prefix "C-SPC"
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

;; COMMENTS
(jep/leader-keys
  :infix ";"
  ";" '(comment-dwim :wk)		
  )					

;; HELP
(jep/leader-keys
  :infix "h"
  "d" '(:ignore t :wk "describe")
  "dv" '(describe-variable :wk)
  "df" '(describe-function :wk)
  "dk" '(describe-key :wk)
  "dg" '(general-describe-keybindings :wk)

  )

;; TOGGLES
(jep/leader-keys
  :infix "t"
  "t" '(counsel-load-theme :which-key)
  "s" '(hydra-text-scale/body :wk)
  )

;; FILES
(jep/leader-keys
  :infix "f"
  "f" '(counsel-find-file :wk)
  "s" '(save-buffer :wk)
  )

;; WINDOW
(jep/leader-keys
  :infix "w"
  "d" '(:wk)
  )

;; SEARCH
 
(jep/leader-keys
  :infix "s"
  "s" '(swiper :wk)


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
  "r"  'magit-rebase)
