;; ======================
;; General Settings
;; ======================

;; Load Custom File
;; (setq custom-file (expand-file-name "custom.el" lisp-d))
;; (condition-case nil
;;     (load custom-file)
;;   (error (with-temp-file custom-file)))

;; Setup Var Dir for Storing Package Generated Files
;; (defvar mjq/var-d "~/.emacs.d/var")

;; Startup Optimization
(setq gc-cons-threshold (* 10 1000 1000));; Faster init
(add-hook 'after-init-hook
  (lambda () (setq gc-cons-threshold (*  2 1000 1000)))) ;; Faster gc pauses

;; Emacs GUI Settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(setq inhibit-startup-screen t)

;; Basic Editing Settings
(show-paren-mode 1)
(save-place-mode 1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(global-visual-line-mode 1)
;; (electric-pair-mode 1) ; automatially inserts matching close paren
(prefer-coding-system 'utf-8)
(global-auto-revert-mode 1) ; make all buffers revert on file changes
(setq ring-bell-function 'ignore) ; shhh....
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)


;; Version Control
(setq version-control t)
(setq backup-by-copying t)
(setq vc-follow-symlinks t)
(setq delete-old-versions t)
(setq vc-make-backup-files t)


;; ======================
;; Package Manager Setup
;; ======================

;; --- straight.el variables (must be set before bootstrap) ---
(setq use-package-always-ensure t)
(setq straight-use-package-by-default t)

;; ;; --- straight.el bootstrap ---
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; --- Setup use-package with straight.el ---
(straight-use-package 'use-package)

;; =================================================================================================
;; (setq coding-system-for-read 'utf-8)
;; (setq coding-system-for-write 'utf-8)
;; (setq sentence-end-double-space nil)    ; sentence SHOULD end with only a point.
;; (setq default-fill-column 80)           ; toggle wrapping text at the 80th character
;; (setq comp-async-report-warnings-errors nil)
;; (setq warning-suppress-log-types '((comp) comp))

;; Otherwise will complain that `ls` does not support `--dired` on osx
;; (when (string= system-type "darwin")
;;   (setq dired-use-ls-dired nil))

;; TODO: Figure out what I want to do with this
;; (setq initial-scratch-message "--- Welcome Back! ---\n\n")
(setq initial-major-mode 'text-mode)  ;; should also help with load times

;; --- Editor Config ---
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(global-visual-line-mode 1)
(show-paren-mode 1)
(prefer-coding-system 'utf-8)

;; --- Font ---
(setq mjq/font-name "SF Mono")
(setq mjq/font-height 120)

;; set font
(set-face-attribute 'default nil
                    :font mjq/font-name
                    :height mjq/font-height)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font mjq/font-name
                    :height mjq/font-height)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font mjq/font-name
                    :height mjq/font-height)

; --- Line Numbers ---
(add-hook 'display-line-numbers-mode-hook
          (lambda () (setq display-line-numbers t)))
(global-display-line-numbers-mode 1)

;; --- No Bells Please ---
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; --- Editor Interation Behavior ---
(defalias 'yes-or-no-p 'y-or-n-p)
;; (setq kill-buffer-query-functions nil)  ;; Removes any question about killing buffers

;; --- Buffer Navigation & Scrolling ---
;; (setq next-screen-context-lines 5)
;; (setq recenter-positions '(top middle bottom))
(setq scroll-margin 5)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)

;; minibuffer interaction
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; --- Backups ---
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
;; (setq create-lockfiles nil)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))


;; ======================
;; Packages
;; ======================
;; --- General Packages ---
;; (use-package f) ;; files
;; (use-package dash) ;; list api
;; (use-package diminish) ;; hide minor modes
(use-package nerd-icons)
(use-package all-the-icons)

;; --- Dashboard ---
(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-init-info t)
  (dashboard-show-shortcuts t)
  (dashboard-icon-type 'all-the-icons)
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-items '((recents . 5)
                     (bookmarks . 5)))
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))

(use-package doom-themes
  :init (load-theme 'doom-one t)
  :custom
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  :config (column-number-mode 1))

(column-number-mode 1)

(defun mjq/open-config ()
  "Open config file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun mjq/load-config ()
  "Reload config file."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; --- Evil ---
(use-package undo-tree
  :diminish
  :init (global-undo-tree-mode))

(use-package evil
  :init (evil-mode)
  :after undo-tree
  :custom
  (evil-undo-system 'undo-tree)
  (evil-want-keybinding nil) ;; necessary for evil-collection
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-emacs-state-cursor '("#fb4933" box)) 
  (evil-normal-state-cursor '("#fabd2f" box))
  (evil-visual-state-cursor '("#83a598" box))
  (evil-insert-state-cursor '("#b8bb26" bar))
  (evil-replace-state-cursor '("#fb4933" box))
  (evil-operator-state-cursor '("#d3869b" hollow))
  :config
  ;; set the colors of the cursor
  (evil-global-set-key 'motion (kbd "j") 'evil-next-visual-line)
  (evil-global-set-key 'motion (kbd "k") 'evil-previous-visual-line)
  ;; (evil-global-set-key 'motion "F" 'avy-goto-char-in-line)
  (evil-set-leader nil (kbd ","))
  (evil-define-key nil (kbd "<escape>") 'global 'keyboard-escape-quit) ; Make ESC quit prompts
  (evil-define-key nil (kbd "<escape>") 'global 'keyboard-escape-quit) ; Make ESC quit prompts
  (evil-define-key '(normal visual operator motion) 'global (kbd "<leader>n") 'treemacs)
  )



(use-package evil-collection
  :after evil
  :init (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer nil)
  (evil-collection-calendar-want-org-bindings t))

(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-mode 1))

(use-package which-key
  :diminish
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.1))


(use-package ivy
  :diminish
  :init (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "[%d / %d] ")
  (ivy-height 15)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-initial-inputs-alist nil))

(use-package swiper
  :after ivy
  :config)

(use-package counsel
  :diminish
  :after ivy
  :init (counsel-mode 1))

(use-package hydra
  :config
  (defhydra hydra-space-leader (:exit t :color blue)
    ("SPC" counsel-M-x "M-x")
    ("p" hydra-projectile/body "Projectile"))
  (evil-global-set-key 'motion (kbd "SPC") 'hydra-space-leader/body))

(use-package evil-escape
  :diminish
  :init (evil-escape-mode 1)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.1)
  (evil-escape-unordered-key-sequence t))


(use-package projectile
  :diminish
  :init (projectile-mode 1)
  :custom
  (projectile-enable-caching t)
  ;; (projectile-cache-file (expand-file-name "projectile.cache" mjq/var-d))
  (projectile-completion-system 'ivy)
  (projectile-project-search-path '("~/Projects/"))
  ;; (projectile-indexing-method 'hybrid)
  :config
  (defhydra hydra-projectile (:color blue)
    ("s" projectile-switch-project "Switch Project")
    ("d" projectile-dired "Project Dired")
    ("r" projectile-invalidate-cache "Refresh Project")
    ("b" projectile-ibuffer "Project Buffers")))

(defhydra hydra-tmux (:hint nil :exit t)
  ("u" evil-window-vsplit)
  ("h" evil-window-split)
  ("x" evil-window-delete)

  ("K" (evil-window-move-very-top))
  ("J" (evil-window-move-very-bottom))
  ("H" (evil-window-move-far-left))
  ("L" (evil-window-move-far-right))

  ("C-k" (shrink-window 10))
  ("C-j" (enlarge-window 10))
  ("C-h" (shrink-window-horizontally 10))
  ("C-l" (enlarge-window-horizontally 10)))

(evil-define-key '(normal insert visual replace operator motion) 'global (kbd "C-t") 'hydra-tmux/body)
(evil-define-key '(normal insert visual replace operator motion) 'global
  (kbd "s-c") 'evil-yank
  (kbd "s-v") 'evil-paste-after)

(evil-define-key
  '(normal insert visual replace operator motion) 'global
  (kbd "C-p") 'projectile-find-file
  (kbd "C-b") 'projectile-display-buffer)

(evil-define-key
  '(normal insert visual replace operator motion) 'global
  (kbd "C-h") 'evil-window-left
  (kbd "C-j") 'evil-window-down
  (kbd "C-k") 'evil-window-up
  (kbd "C-l") 'evil-window-right)

(use-package flycheck
  :diminish
  :custom
  (flycheck-python-pylint-executable "pylint"))

(use-package magit)

(use-package evil-surround
  :init (global-evil-surround-mode 1))
;; (defun image-type-available-p (type)
;;   "Return t if image type TYPE is available.
;;   Image types are symbols like `xbm' or `jpeg'."
;;   (if (eq 'svg type)
;;       nil
;;       (and (fboundp 'init-image-library)
;;           (init-image-library type))))

(add-to-list 'image-types 'svg) ;; fixes bug (will be fixed in emacs 29)
(use-package treemacs
  :config)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)


;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))


;; 
;; ;; --- Dired ---
;; (use-package dired
;;   :ensure nil
;;   :config
;;   (add-hook 'dired-load-hook (lambda () (load "dired-x")))
;;   (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
;; 
;; (use-package all-the-icons-dired)
;; 
;; ;; --- Evil Leader Def ---
;; ;; Using evil-leader over hydra for easy mode-specific keybindings
;; ;; Not sure if this actually makes sense to do...
;; ;; (use-package evil-leader)
;; ;; (global-evil-leader-mode)
;; ;; (evil-leader/set-leader ",")
;; ;; (evil-leader/set-key "n" 'neotree-toggle)
;; 
;; 
;; ;; --- Org Mode ---
;; ;; (use-package org-bullets
;; ;;   :config
;; ;;   (add-hook 'org-mode-hook 'org-bullets-mode))
;; 
;; ;; (use-package evil-org
;; ;;   :config
;; ;;   (add-hook 'org-mode-hook 'evil-org-mode)
;; ;;   (evil-org-set-key-theme
;; ;;    '(navigation insert textobjects additional calendar)))
;; 
;; ;; (setq org-special-ctrl-a/e t)
;; ;; (setq org-todo-keywords
;; ;;       '((sequence "TODO" "INPROGRESS" "WAITING" "|" "DONE")))
;; ;; (setq org-todo-keyword-faces
;; ;;       '(("TODO" . "red")
;; ;;         ("INPROGRESS" . "orange")
;; ;;         ("WAITING" . "blue")))
;; 
;; ;; (setq org-log-done 'time)
;; ;; (setq org-hierarchical-todo-statistics nil)
;; 
;; ;; (evil-define-key
;; ;;   nil 'ivy-minibuffer-map
;; ;;   "C-j" 'ivy-next-line
;; ;;   "C-k" 'ivy-previous-line
;; ;;   )
;; 
;; ;; (evil-define-key
;; ;;   '(normal) 'org-mode-map
;; ;;   "t" 'org-todo
;; ;;   "-" 'org-cycle-list-bullet
;; ;;   "H" 'org-insert-heading-respect-content
;; ;;   "T" 'org-insert-todo-heading-respect-content
;; ;;   )
;; 
;; 
;; ;; (evil-define-key
;; ;;   '(normal) 'org-mode-map
;; ;;   "t" 'org-todo
;; ;;   "-" 'org-cycle-list-bullet
;; ;;   "H" 'org-insert-heading-respect-content
;; ;;   "T" 'org-insert-todo-heading-respect-content
;; ;;   )
;; 
;; ;; Org Mode Keybindings
;; ;; (evil-leader/set-key-for-mode 'org-mode
;; ;;   "t" 'org-todo
;; ;;   "T" 'org-show-todo-tree)
;; 
;; ;; (evil-define-key
;; ;;   '(normal visual operator motion) 'mjq-intercept-mode-map
;; ;;   (kbd ",") 'hydra-evil-leader/body)
