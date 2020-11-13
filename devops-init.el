;; package --- bsnux-emacs configuration for DevOps

;;; Commentary:

;; /ssh:user@host:file   tramp
;; /sudo:file            sudo editing

;;; Code:

;; Setting use-package
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Packages configuration stars there
(use-package evil
  :config
  (evil-mode 1)
  (setq evil-vsplit-window-right t)
  :bind
  (:map evil-insert-state-map
        ("C-a" . move-beginning-of-line)
        ("C-e" . move-end-of-line)
        ("C-k" . kill-line)
        ("C-w" . kill-region)
        ("C-y" . yank))
  (:map evil-visual-state-map
        ("C-a" . move-beginning-of-line)
        ("C-e" . move-end-of-line))
  (:map evil-normal-state-map
        ("C-k" . kill-line)
        ("C-y" . yank))
  :ensure t)

(use-package evil-terminal-cursor-changer
  :config
  (evil-terminal-cursor-changer-activate)
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; ⎸
  (setq evil-emacs-state-cursor  'hbar) ; _
  :ensure t)

(use-package solarized-theme
  :load-path "themes"
  ;;:config
  ;;(load-theme 'solarized-zenburn t)
  :ensure t)

(use-package base16-theme
  :ensure t)

(use-package atom-one-dark-theme
  :load-path "themes"
  ;;:config
  ;;(load-theme 'atom-one-dark t)
  :ensure t)

(use-package editorconfig
   :ensure t)

(use-package json-mode
   :ensure t)

(use-package dockerfile-mode
   :ensure t)

(use-package yaml-mode
   :ensure t)

(use-package groovy-mode
   :ensure t)

(use-package flycheck
  :config
  (global-flycheck-mode)
  :ensure t)

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :ensure t)

(use-package neotree
   :ensure t)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
  :ensure t)

(use-package helm
  :config
  (helm-mode 1)
  :bind
  ("C-x C-f" . helm-find-files)
  ("s-p" . helm-buffers-list)
  (:map helm-map
	("<tab>" . helm-execute-persistent-action))
  :ensure t)

(use-package company
  :config
  (global-company-mode t)
  :bind
  (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :ensure t)

;; core settings
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(setq linum-format "%d ")
(global-linum-mode t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq visible-bell nil)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(setq tramp-default-method "ssh")
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
(delete-selection-mode 1)
(show-paren-mode 1)
(load-theme 'solarized-zenburn t)
;; Disabling bold fonts
(mapc
 (lambda (face)
        (when (eq (face-attribute face :weight) 'bold)
          (set-face-attribute face nil :weight 'normal)))
 (face-list))
;; Only for UI
(when window-system
  (add-to-list 'default-frame-alist '(height . 75))
  (add-to-list 'default-frame-alist '(width . 120))
  (copy-face 'italic 'font-lock-comment-face)
  (set-face-foreground 'font-lock-comment-face "#6a1a09d")
  (set-face-attribute 'default nil :font "Hack-15")
  (set-frame-font "Hack-15" nil t)
  (load-theme 'base16-oceanicnext t))

;;; init.el ends here
