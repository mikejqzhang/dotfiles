;; general settings
(setf inhibit-startup-screen t) ; disable welcome screen
(setf ring-bell-function 'ignore) ; disable alarm bell
(setq-default visible-bell t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(display-time-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(show-paren-mode 1) ; highlight matching parens
(global-hl-line-mode 1) ; highlight current line
(setq-default indent-tabs-mode nil) ; use spaces instead of tabs
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
