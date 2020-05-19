;; ======================
;; Path Setup
;; ======================
;; --- Relevent Dirs ---
(defvar emacs-d
  (file-name-directory
    (file-chase-links load-file-name))
  "Where the Wild Things Are...")
(defvar lisp-d
  (expand-file-name "lisp" emacs-d)
  "Personal elisp files loaded by init.el")
(defvar var-d
  (expand-file-name "var" emacs-d)
  "Dir to place untracked stuff generated by emacs")
(add-to-list 'load-path lisp-d)

;; --- Load Custom File ---
(setq custom-file (expand-file-name "custom.el" lisp-d))
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

;; Make Emacs Start in Fullscreen
(add-hook 'after-frame-make-functions
          '(lambda ()
             (unless (frame-parameter nil 'fullscreen)
                     (toggle-frame-fullscreen))))
(add-hook 'after-init-hook
          '(lambda ()
             (unless (frame-parameter nil 'fullscreen)
                     (toggle-frame-fullscreen))))

;; --- Editor Config ---
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(setq truncate-lines t)  ;; no line wrapping
(show-paren-mode 1)
(prefer-coding-system 'utf-8)

; --- Font ---
(set-face-attribute 'default nil :height 140)
(defun mjq-set-font (&optional frame)
  "Set font to SF Mono if availible in the current FRAME."
  (when frame
    (select-frame frame))
  (condition-case nil
    (set-frame-font "SF Mono")
    (error nil)))
(add-hook 'after-frame-make-functions 'mjq-set-font)
(add-hook 'after-init-hook 'mjq-set-font)

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

;; minibuffer interaction
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

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

;; ======================
;; Packages
;; ======================
;; --- General Packages ---
;; string maniuplation
(use-package s)
;; files
(use-package f)
;; list api
(use-package dash)
;; hide minor modes
(use-package diminish)
;; TODO: Bootstrap all-the-icons to run (all-the-icons-install-fonts)
;; pretty icons everywhere
(use-package all-the-icons)
;; --- Dashboard ---

;; --- Startup Packages ---
;; TODO: Can I use this in org?
;; I think this makes dashed lines across a buffer
(use-package page-break-lines
  :diminish)  ;; required for dashboard
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
  (setq dashboard-set-navigator t) ;; Use this if I ever want to setup links
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-items '((projects . 8)
                          (bookmarks . 8))))

;; --- Evil ---
(use-package undo-tree
  :diminish)

;; evil soft dependency
;; For the motions g; g, and for the last-change-register .
(use-package goto-chg)

(use-package evil
  :init
  ;; both are required for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :after (undo-tree goto-chg)
  :config
  ;; TODO: Decide whether or not I want this behavior
  ;; Disables entering emacs state when opening certain modes
  (setq evil-auto-balance-windows nil)
  (setq evil-emacs-state-modes nil)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; --- Magit ---
(use-package magit)

(use-package evil-magit
  :after (evil magit)
  ;; TODO: See github page about setting this
  ;; :init
  ;; (setq evil-magit-use-y-for-yank nil)
  )

;; --- Dired ---
(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-load-hook (lambda () (load "dired-x")))
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-dired)

;; --- Keybindings ---
(use-package which-key
  :diminish
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.1))

(use-package hydra)

;; Only used for jk esc keybinding
(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-mode 1))

;; --- Ivy/Counsel ---
(use-package ivy
  :diminish
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "[%d / %d] ")
  (setq ivy-height 15)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1)
  )

;; TODO: Look into these?
;; (use-package ivy-hydra)

(use-package swiper
  :after ivy
  :config
  )

(use-package counsel
  :diminish
  :after swiper
  :config
  (counsel-mode)
  )


(use-package projectile
  :diminish
  :config
  (setq projectile-enable-caching t)
  (setq projectile-cache-file (expand-file-name "projectile.cache" var-d))
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/Projects/"))
  (setq projectile-indexing-method 'hybrid)
  (projectile-mode 1)
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
  (setq neo-smart-open t))

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

(use-package flycheck
  :diminish
  :config
  (setq flycheck-python-pylint-executable "pylint")
  (global-flycheck-mode))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after anaconda-mode)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backends
    '(company-files
      company-keywords
      company-capf
      )))

(use-package persp-mode
  :config
  (with-eval-after-load "persp-mode-autoloads"
      ;; (setq wg-morph-on nil) ;; switch off animation
      (setq persp-autokill-buffer-on-remove 'kill-weak)
      (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
  (with-eval-after-load "persp-mode"
    (with-eval-after-load "ivy"
        (add-hook 'ivy-ignore-buffers
                #'(lambda (b)
                    (when persp-mode
                        (let ((persp (get-current-persp)))
                        (if persp
                            (not (persp-contain-buffer-p b persp))
                            nil)))))

        (setq ivy-sort-functions-alist
            (append ivy-sort-functions-alist
                    '((persp-kill-buffer   . nil)
                        (persp-remove-buffer . nil)
                        (persp-add-buffer    . nil)
                        (persp-switch        . nil)
                        (persp-window-switch . nil)
                        (persp-frame-switch  . nil))))))
  )


;; ======================
;; Keybindings
;; ======================

;; --- High Precedence Personal Keymap ---
;; General use intercept map.
;; For use with evil-define-key to define which evil states to override
(defvar mjq-intercept-mode-map (make-sparse-keymap)
  "High precedence personal keymap.")
(define-minor-mode mjq-intercept-mode
  "Global minor mode for higher precedence personal evil keybindings."
  :global t)
(dolist (state '(normal insert visual replace operator motion))
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap mjq-intercept-mode-map state t t)
   state))
(mjq-intercept-mode)

;; Unfortunate, but this must be done for "jk" esc keybinding
;; key-chord doesn't play nice with evil-define-key :( 
(defvar mjq-intercept-esc-mode-map (make-sparse-keymap)
  "High precedence personal keymap.")
(define-minor-mode mjq-intercept-esc-mode
  "Global minor esc-mode for higher precedence personal evil keybindings."
  :global t)
(dolist (state '(normal insert visual replace operator motion))
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap mjq-intercept-esc-mode-map state t t)
   state))
(mjq-intercept-esc-mode)

;; --- General Editing Behavior ---
(evil-define-key
  'visual 'global
  (kbd "C-c") 'evil-normal-state)
;; (evil-define-key 
;;   '(normal insert visual replace operator motion) 'global
;;   (kbd "s-x") 'counsel-M-x)

;; --- Defining jk/kj Escape Behavior (with keymap) ---
(key-chord-define mjq-intercept-esc-mode-map "jk" 'evil-normal-state)

;; --- Window Managment ---
(evil-define-key
  '(normal insert visual replace operator motion) 'mjq-intercept-mode-map
  (kbd "C-h") 'evil-window-left
  (kbd "C-j") 'evil-window-down
  (kbd "C-k") 'evil-window-up
  (kbd "C-l") 'evil-window-right)

(defhydra hydra-window-manager (:color blue
                                :idle 0.5)
  ("u" evil-window-vsplit)
  ("h" evil-window-split)
  ("x" evil-window-delete)

  ("C-k" (shrink-window 5) :exit nil)
  ("C-j" (enlarge-window 5) :exit nil)
  ("C-h" (shrink-window-horizontally 5) :exit nil)
  ("C-l" (enlarge-window-horizontally 5) :exit nil))

(evil-define-key
  '(normal insert visual replace operator motion) 'mjq-intercept-mode-map
  (kbd "C-t") 'hydra-window-manager/body)

;; --- Project/Buffer Navigation ---
(evil-define-key 
  '(normal insert visual replace operator motion) 'mjq-intercept-mode-map
  (kbd "C-p") 'projectile-find-file)
(evil-define-key 
  '(normal insert visual replace operator motion) 'mjq-intercept-mode-map
  (kbd "C-b") 'projectile-display-buffer)

(defhydra hydra-projectile (:color blue
                            :idle 0.5)
  ("s" projectile-switch-project)
  ("d" projectile-dired)
  ("r" projectile-invalidate-cache)
  ("b" projectile-ibuffer)
  )
;; TODO: Do I want this?
;; (define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-r") 'projectile-invalidate-cache)


;; Functions
(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file"))))

;; --- Space Leader Def ---
;; This leader should provide some global features
;; Orgnized thematicalls or by package
(defhydra hydra-space-leader (:color blue
                              :idle 0.5)
  ("SPC" counsel-M-x)
  ("p" hydra-projectile/body))
(evil-define-key
  '(normal visual operator motion) 'mjq-intercept-mode-map
  (kbd "SPC") 'hydra-space-leader/body)

;; --- Evil Leader Def ---
;; TODO: Decide whether this should be a thing?
;; This leader's behavior that may overridden based on current active mode
;; such modes should define a cond clause above to be added here
;; Example:
;; ((MODE_CONDITION)
;;     (prog1
;;         (eval (hydra--format nil '(nil nil :hint nil)
;;                              "\neven: _j_ _k_\n" hydra-NAME/heads))
;;         (define-key hydra-NAME/keymap "key" COMMAND_OR_NIL)))

(defhydra hydra-evil-leader (:color blue
                             :idle 0.5
                             :hint nil)
  ("n" neotree-toggle))
(evil-define-key
  '(normal visual operator motion) 'mjq-intercept-mode-map
  (kbd ",") 'hydra-evil-leader/body)
