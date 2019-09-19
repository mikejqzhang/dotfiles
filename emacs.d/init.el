;; Leave this here, or package.el will just add it again.
(package-initialize)

;; setting up stuff
(require 'package)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; set up personal lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; set up themes directory
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

;; single function call for reloading emacs config in emacs
(defun reload-init ()
  "Reload the init file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (spaceline-compile))

;; general settings
(setf inhibit-startup-screen t) ; disable welcome screen
(setf ring-bell-function 'ignore) ; disable alarm bell
(setq visible-bell t)
;; (setq-default bell-inhibit-time 10)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(display-time-mode 1)
(show-paren-mode 1) ; highlight matching parens
(global-hl-line-mode 1) ; highlight current line
(setq indent-tabs-mode nil) ; use spaces instead of tabs
(fset 'yes-or-no-p 'y-or-n-p)
(set-face-attribute 'default nil :height 140) ; set font size

;; improve scrolling
(setf scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

(add-hook 'display-line-numbers-mode-hook
                   (lambda () (setq display-line-numbers-type 'relative)))
         (add-hook 'prog-mode-hook #'display-line-numbers-mode)

(global-display-line-numbers-mode)

;; sets up package archives
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t) ;; kinda redundant in currant setup

(use-package auto-compile
  :config
  (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Package configs
(use-package diminish
  :ensure t)

(use-package dash
  :ensure t)

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.1)
  )

(use-package general
  :ensure t
  :after which-key
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)
  )

;; must be setup before evil
(use-package undo-tree
  :diminish
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
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

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t)


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
  :ensure t
  :config
  (projectile-mode 1)
  ;; (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (setq projectile-project-search-path '("~/projects/"))
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
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(use-package powerline
  :ensure t
  :config
  )

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq powerline-default-separator 'wave
        spaceline-workspace-numbers-unicode t
        spaceline-window-numbers-unicode t)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode)
  (spaceline-info-mode)
  (setq powerline-default-separator 'arrow)
  )

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
  (setq-default flycheck-flake8-maximum-line-length 100)

  )

(use-package flyspell-correct
  :ensure t
  :config
  ;; (setq ispell-list-command "--list")
  )

(use-package flyspell-correct-helm
  :bind ("C-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-helm)
  )


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

;; (use-package column-marker
;;   :ensure n
;;   :load-path "lisp/column-marker.el"
;;   :config
;;     (add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80)))
;;     )

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
(setq org-cycle-separator-lines 1)

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

