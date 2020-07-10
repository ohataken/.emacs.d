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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package dired-subtree
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  :bind (:map dired-mode-map
    ("i" . dired-subtree-toggle)))

(use-package expand-region
  :bind (
    ("C-c e" . er/expand-region)))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package pallet
  :config
  (pallet-mode t))

(use-package projectile
  :init
  (setq projectile-known-projects-file (locate-user-emacs-file "tmp/projectile-bookmarks.eld"))
  :bind (
    ("C-x C-f" . projectile-find-file))
  :config
  (projectile-mode))

(use-package saveplace
  :init
  (setq save-place-file (locate-user-emacs-file "tmp/places"))
  :config
  (save-place-mode 1))

(use-package smartparens
  :config
  (smartparens-global-mode t))

(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :config
  (global-company-mode t))

(use-package dired-subtree
  :init
  (setq dired-subtree-use-background nil))

(use-package golden-ratio
  :init
  (setq golden-ratio-auto-scale t)
  :config
  (golden-ratio-mode t))

(use-package iflipb
  :init
  (setq iflipb-ignore-buffers nil)
  :bind (
    ("M-h" . iflipb-next-buffer)
    ("M-H" . iflipb-previous-buffer)))

(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 1)      
  :config
  (smooth-scrolling-mode t))

(use-package magit)

(use-package counsel
  :config
  (counsel-mode 1))

(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(global-auto-revert-mode t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq debug-on-error t)
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)
(setq custom-file (locate-user-emacs-file "tmp/custom.el"))
(setq suggest-key-bindings t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines t)
(setq-default inhibit-startup-screen t)
(setq-default inhibit-splash-screen t)
(global-linum-mode t)
