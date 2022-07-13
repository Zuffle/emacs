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

(use-package counsel
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))

(straight-use-package 'spaceline)
(use-package spaceline
  :config
  (spaceline-emacs-theme)
  )
;; (require 'spaceline-config)


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
  :init
  (ivy-rich-mode))

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

(use-package general
  ;; (general-evil-setup t)
  )
(general-setq source-directory "c:/Program Files/Emacs/emacs-27.2")

;; (straight-use-package
;;  '(evil
;;    :pre-build (
;; 	       (general-setq evil-want-integration t)
;; 	       (general-setq evil-want-keybinding nil)
;; 	       (general-setq evil-wantC-u-scroll t)
;; 	       )
;;    :post-build (
;; 		(message "post-build") 
;; 		)
;;    )
;;  )

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
;; (straight-use-package
;;  '(evil-escape
;;    (evil-escape-mode 1)
;;    ))

(use-package evil-escape
  :config
  (evil-escape-mode 1)
  )

(use-package howm
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings		  	     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (general-def
;;   :prefix "SPC"
;;   "" nil
;;   ";;" '(comment-line :which-key "comment line"))



(general-create-definer jep/leader-keys
  :keymaps '(normal insert emacs)
  :prefix "SPC"
  :non-normal-prefix "C-SPC"
  )

(jep/leader-keys
  ;; prefix key setup
  "f" '(:ignore t :wk "files")
  "t" '(:ignore t :wk "toggles")
  "a" '(:ignore t :wk "applications")
  "w" '(:ignore t :wk "window")


  "tt" '(counsel-load-theme :which-key "choose theme")
  "ff" '(counsel-find-file :wk "find-file")
  )
