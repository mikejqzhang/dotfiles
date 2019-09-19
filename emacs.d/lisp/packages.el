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
