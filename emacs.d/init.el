;; ======================
;; Path Setup
;; ======================
;; --- Move cutsom file ---
(setq custom-file (locate-user-emacs-file "custom.el"))
(setq custom-file "~/.emacs.d/custom.el")
(condition-case nil
    (load custom-file)
  (error (with-temp-file custom-file)))

;; ======================
;; General Settings
;; ======================
;; --- Startup Optimization  ---
(setq gc-cons-threshold (* 1024 1024 100))

;; --- Disable GUI Elements (Also Helps With Startup) ---
;; from http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(hidden-mode-line-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)

;; --- Startup Screen ---
(setq inhibit-startup-screen t)
;; TODO: Figure out what I want to do with this
;; (setq initial-scratch-message "--- Welcome Back! ---\n\n")
(setq initial-major-mode 'text-mode)  ;; should also help with load times

;; --- Editor Config ---
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(setq truncate-lines t)  ;; no line wrapping
(show-paren-mode 1)
(prefer-coding-system 'utf-8)

; --- Font ---
(set-face-attribute 'default nil :height 130)
(defun mjq-set-font (&optional frame)
  (when frame
    (select-frame frame))
  (condition-case nil
    (set-frame-font "SF Mono")
    (error nil)))
(mjq-set-font)
(add-hook 'after-frame-make-functions 'mjq-set-font)

; --- Line Numbers ---
(add-hook 'display-line-numbers-mode-hook
          (lambda () (setq display-line-numbers t)))
(global-display-line-numbers-mode)

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


;; --- Link Navigation
;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
;; (add-hook 'prog-mode-hook 'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
;; (add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; Keep focus while navigating help buffers
;; (setq help-window-select 't)

;; Scroll compilation to first error or end
;; (setq compilation-scroll-output 'first-error)

;; Where to start line-wrapping
;; (setq-default fill-column 80)

;; Save clipboard contents into kill-ring before replace them
;; (setq save-interprogram-paste-before-kill t)

;; --- Backups ---
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
;; (setq create-lockfiles nil)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))


;; use only spaces and no tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;; ======================
;; Bootstrap
;; ======================
;; Initialize package.el
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
;; Boostrp use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-verbose t))

;; Package configs
(use-package dash)
(use-package diminish)
;; TODO: Bootstrap all-the-icons to run (all-the-icons-install-fonts)
(use-package all-the-icons)
(use-package page-break-lines :diminish)  ;; required for dashboard
(use-package dashboard
  :after page-break-lines
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title
        "[ Research is formalized curiosity - Zora Neale Hurston ]")
  (setq dashboard-footer-messages
        '("The one true editor, Emacs!"
        "Who the hell uses VIM anyway? Go Evil!"
        "Free as free speech, free as free Beer"
        "Happy coding!"
        "Vi Vi Vi, the editor of the beast"
        "Welcome to the church of Emacs"
        "While any text editor can save your files, only Emacs can save your soul"
        "I showed you my source code, pls respond"))
  ;; (setq dashboard-set-navigator t) ;; Use this if I ever want to setup links
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-items '((projects . 8)
                          (bookmarks . 8))))

(use-package which-key
  :diminish
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.1))

(use-package general
  :after which-key
  :config
  (general-evil-setup)
  (general-auto-unbind-keys))


(use-package undo-tree
  :diminish)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :after undo-tree
  :config
  (evil-mode 1)
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package all-the-icons-dired)

(use-package whitespace
  :ensure nil
  :diminish
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (global-whitespace-mode 1)
  )

(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

(use-package magit
  :ensure t
  )

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  )

(use-package projectile
  :diminish
  :config
  (projectile-mode 1)
  ;; (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (setq projectile-project-search-path '("~/Projects/"))
  (setq projectile-indexing-method 'hybrid)
  )

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  )

(use-package helm-flx
  :ensure t
  :config
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t ;; t by default
        helm-flx-for-helm-locate t) ;; nil by default
  )


(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  (setq neo-smart-open t)
  :general
  (general-def 'normal neotree-mode-map
    "<RET>" 'neotree-enter
    "ll" 'neotree-hidden-file-toggle
    "f" 'neotree-stretch-toggle
    "U" 'neotree-select-up-node
    "R" 'neotree-refresh
    ">" 'neotree-change-root
    "c" 'neotree-create-node
    "dd" 'neotree-delete-node))

(use-package spacemacs-common
  :ensure spacemacs-theme
  :init
  (setq spacemacs-theme-comment-bg nil)
  :config
  (load-theme 'spacemacs-dark t))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; --- Mode Line ---
(use-package spaceline
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'wave)
  :config
  (spaceline-spacemacs-theme)
  )
;; Broken? Use when it works...
;; (use-package spaceline-all-the-icons 
;;   :after spaceline
;;   :config
;;   )


;; (use-package spaceline-config
;;   :init
;;   ;; (setq powerline-default-separator 'wave)
;;   ;; ;;       spaceline-workspace-numbers-unicode t
;;   ;; ;;       spaceline-window-numbers-unicode t)
;;   ;; ;; (spaceline-helm-mode)
;;   ;; ;; (spaceline-info-mode)
;;   (require 'spaceline)
;;   ;; (spaceline-compile)
;;   :config
;;   (spaceline-spacemacs-theme)
;;   )
;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

;; (use-package company-jedi
;;   :config
;;   (setq jedi:environment-virtualenv
;;         (list (expand-file-name "~/.emacs.d/python-environments/")))
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)
;;   (defun config/enable-company-jedi ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'config/enable-company-jedi)
;;   )

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq-default flycheck-flake8-maximum-line-length 80)

  )

;; (use-package flyspell-correct
;;   :ensure t
;;   :config
;;   ;; (setq ispell-list-command "--list")
;;   )
;; 
;; (use-package flyspell-correct-helm
;;   :bind ("C-;" . flyspell-correct-wrapper)
;;   :init
;;   (setq flyspell-correct-interface #'flyspell-correct-helm)
;;   )


(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

;; TODO: Fix the line wrap "%" problem
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/usr/local/bin/zsh"))

;; (use-package evil-org
;;   :ensure t
;;   )

;; (require 'column-marker)
;; (add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 100)))

;; (use-package column-marker
;;   :ensure n
;;   :config
;;   (add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 100))))

(diminish 'global-whitespace-mode)
(diminish 'auto-revert-mode)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this)
  (global-set-key (kbd "s-D") 'mc/mark-previous-like-this)
  (global-set-key (kbd "s-G") 'mc/mark-all-like-this)
  )

(use-package eyebrowse
  :diminish
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-close-window-config-prompt t)
  (setq eyebrowse-keymap-prefix "")
  (setq eyebrowse-mode nil)
  (setq eyebrowse-mode-line-style (quote always))
  (setq eyebrowse-slot-format "%s: untitled")
  (setq eyebrowse-tagged-slot-format "%s: %t")
  )

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

(general-create-definer tmux-leader-def
  :states 'normal
  :keymaps 'override
  :prefix "C-t")

(setq evil-auto-balance-windows nil)

(tmux-leader-def
  "u" 'evil-window-vsplit
  "h" 'evil-window-split

  "x" 'evil-window-delete

  "H" 'evil-window-move-far-left
  "J" 'evil-window-move-very-bottom
  "K" 'evil-window-move-very-top
  "L" 'evil-window-move-far-right

  "C-k" '(lambda () (interactive) (shrink-window 7))
  "C-j" '(lambda () (interactive) (enlarge-window 7))
  "C-h" '(lambda () (interactive) (shrink-window-horizontally 7))
  "C-l" '(lambda () (interactive) (enlarge-window-horizontally 7))

  "1" 'eyebrowse-switch-to-window-config-1
  "2" 'eyebrowse-switch-to-window-config-2
  "3" 'eyebrowse-switch-to-window-config-3
  "4" 'eyebrowse-switch-to-window-config-4
  "5" 'eyebrowse-switch-to-window-config-5
  "6" 'eyebrowse-switch-to-window-config-6
  "7" 'eyebrowse-switch-to-window-config-7
  "8" 'eyebrowse-switch-to-window-config-8
  "9" 'eyebrowse-switch-to-window-config-9
  "0" 'eyebrowse-switch-to-window-config-0
  "l" 'eyebrowse-last-window-config
  ">" 'eyebrowse-next-window-config
  "<" 'eyebrowse-prev-window-config
  "," 'eyebrowse-rename-window-config
  "c" 'eyebrowse-create-window-config
  "k" 'eyebrowse-close-window-config
  "w" 'eyebrowse-switch-to-window-config
  )

(general-def
  :states '(normal visual insert)
  :keymaps 'override
  "C-h" 'evil-window-left
  "C-j" 'evil-window-down
  "C-k" 'evil-window-up
  "C-l" 'evil-window-right
  )


(general-create-definer evil-leader-def
  :states 'normal
  ;; :keymaps 'override
  :prefix ",")

(evil-leader-def
  "n" 'neotree-toggle
  "d" 'dired
  )

;; key-chord like behavior for seeing esc
(general-def
  :states '(insert replace)
  "k" (general-key-dispatch 'self-insert-command
        :timeout 0.1
        "j" 'evil-normal-state)
  "j" (general-key-dispatch 'self-insert-command
        :timeout 0.1
        "k" 'evil-normal-state))

(general-def
  :states 'visual
  "C-c" 'evil-normal-state)

(general-def
  "M-x" 'helm-M-x
  "M-h"  help-map
  )

(general-nmap
  :keymaps 'override
  "C-p" 'helm-projectile
  "C-b" 'helm-projectile-switch-to-buffer
  )

(general-create-definer space-leader-def
  :states '(normal visual)
  :keymaps 'override
  :prefix "SPC")

(space-leader-def
  "SPC" 'helm-M-x
  "p" (general-key-dispatch 'self-insert-command
        :which-key "projectile"
        "p" 'helm-projectile
        "f" 'helm-projectile-recentf
        "b" 'helm-mini
        "s" 'projectile-switch-project
        "r" 'projectile-invalidate-cache
        "!" 'projectile-run-shell-command-in-root
        "d" 'projectile-dired
        )
  "g" (general-key-dispatch 'self-insert-command
        :which-key "git"
        "r" 'magit-refresh-all
        "s" 'magit-status
        "i" 'magit-gitignore
        )
  "h" (general-key-dispatch 'self-insert-command
        :which-key "helm"
        "k" 'helm-show-kill-ring
        "r" 'helm-register
        )
  "o" (general-key-dispatch 'self-insert-command
        :which-key "org"
        "a" 'org-agenda
        "b" 'org-switchb
        "c" 'org-capture
        "l" 'org-store-link
        )
  "?" help-map
  "!" 'shell-command
  )

(general-def
  :keymaps 'magit-status-mode-map
  "<tab>" 'magit-section-cycle
  "f f" 'magit-fetch-all
  )

(general-def
  "M-x" 'helm-M-x
  "s-x" 'helm-M-x
  "M-h"  help-map
  "s-h"  help-map
  )

(general-def
  :keymaps 'helm-map
  "<tab>" 'helm-execute-persistent-action
  "C-z"  'helm-select-action
  "C-h"  'helm-previous-page
  "C-j"  'helm-next-line
  "C-k"  'helm-previous-line
  "C-l"  'helm-next-page
  )

(setq org-special-ctrl-a/e t)

;; allows for newline under headings
;; (setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))
;; (setq org-cycle-separator-lines 1)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING"
                  "|" "DONE" "CANCELED")))

(setq org-enforce-todo-dependencies t)

(setq org-log-done 'time)

(setq org-hierarchical-todo-statistics nil)
(setq org-src-preserve-indentation nil 
      org-edit-src-content-indentation 0)


(defun clever-insert-item ()
  "Clever insertion of org item."
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item))
  )

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function.
FUN function callback"
  (end-of-line)
  (funcall fun)
  (evil-append nil)
  )

(general-nmap
  :keymaps 'org-mode-map
  "t" 'org-todo
  "s-t" '(lambda () (interactive) (evil-org-eol-call (lambda() (org-insert-todo-heading nil))))
  "-" 'org-cycle-list-bullet
  "0" 'org-beginning-of-line
  "$" 'org-end-of-line
  "o" '(lambda () (interactive) (evil-org-eol-call 'clever-insert-item))
  "s-o" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "H" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading-respect-content))
  "T" '(lambda () (interactive) (evil-org-eol-call 'org-insert-todo-heading-respect-content))
  ">>" 'org-do-demote
  "<<" 'org-do-promote
  "s-." 'org-demote-subtree
  "s-," 'org-promote-subtree
  "<tab>" 'org-cycle
  "s-h" 'org-table-move-column-left
  "s-j" 'org-drag-element-forward
  "s-k" 'org-drag-element-backward
  "s-l" 'org-table-move-column-right
  )

;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

(general-nmap
  :keymaps 'calendar-mode-map
  "C-h" 'calendar-backward-day
  "C-j" 'calendar-forward-week
  "C-k" 'calendar-backward-week
  "C-l" 'calendar-forward-day
  )

(evil-leader-def
  :keymaps 'org-mode-map
  "t" 'org-show-todo-tree
  "s" 'org-schedule
  "d" 'org-deadline
  "r" 'org-mode-restart
  )

(modify-syntax-entry ?_ "w")
;; (visual-line-mode)

;; Functions
(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))))

(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))

