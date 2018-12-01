(add-to-list 'load-path "~/.emacs.d/vendor/use-package")
(require 'use-package)

;; this needs to be loaded before evil
(use-package undo-tree
  :load-path "vendor/undo-tree")

;; evil mode
(use-package evil
  :load-path "vendor/evil"

  :init
	(setf evil-want-C-u-scroll t)
  (setf evil-want-fine-undo t)
  (setf evil-want-abbrev-expand-on-insert-exit nil)


  :config
  ;; enable evil mode
  (evil-mode 1)

	(defun minibuffer-keyboard-quit ()
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setf deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
	(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (global-set-key (kbd "RET") 'newline-and-indent)
	;; key bindings
  (evil-ex-define-cmd "!" 'shell-command)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map ";" 'buffer-menu)

	)


;; this needs to be loaded after evil
(use-package neotree
  :load-path "vendor/emacs-neotree"

  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter))))

;; this needs to be loaded after evil and neotree
(use-package evil-leader
  :load-path "vendor/evil-leader"

  :config
  (global-evil-leader-mode)
  (evil-leader/set-key
    "m" 'menu-bar-open
    "n" 'neotree-toggle
    "f" 'neotree-find))
