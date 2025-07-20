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

(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/bin/bash"))

(use-package multi-vterm
  :ensure t)

(use-package projectile
  :init
  (setq projectile-known-projects-file (locate-user-emacs-file "tmp/projectile-bookmarks.eld")))

(use-package counsel-projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode t))

(use-package smartparens
  :config
  (smartparens-global-mode t))

(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 1)
  :config
  (smooth-scrolling-mode t))

(use-package magit
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :init
  (counsel-mode 1)
  :config
  (setq counsel-projectile-preview-buffers t)
  (setq counsel-switch-buffer-preview-virtual-buffers t)
  :bind (
    "C-x b" . counsel-switch-buffer))

(use-package ivy-rich
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :config
  (custom-set-variables '(prescient-save-file (locate-user-emacs-file "tmp/prescient-save.el")))
  (ivy-prescient-mode t)
  (prescient-persist-mode t))

(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(global-auto-revert-mode t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq debug-on-error t)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(menu-bar-mode -1)
(save-place-mode 1)
(recentf-mode)
(setq recentf-auto-cleanup 'never)
(setq custom-file (locate-user-emacs-file "tmp/custom.el"))
(setq recentf-save-file (locate-user-emacs-file "tmp/recentf"))
(setq transient-history-file (locate-user-emacs-file "tmp/transient-history.el"))
(setq save-place-file (locate-user-emacs-file "tmp/places"))
(setq suggest-key-bindings t)
(setq js-indent-level 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines t)
(setq-default inhibit-startup-screen t)
(setq-default inhibit-splash-screen t)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
