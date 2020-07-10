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
