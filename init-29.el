;; package --- bsnux Emacs 29.x config

;;; Commentary:

;; /ssh:user@host:file   tramp
;; /sudo:file            sudo editing

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Hide UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Better default modes
(electric-pair-mode t)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)
(global-display-line-numbers-mode t)

;; Disabling bold fonts
(mapc
 (lambda (face)
        (when (eq (face-attribute face :weight) 'bold)
          (set-face-attribute face nil :weight 'normal)))
 (face-list))

;; scratch buffer by default
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; scratch buffer: Lisp evaluation & draft notes")

;; treesitter: M-x treesit-install-language-grammar 
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Better default settings
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq visible-bell nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq tramp-default-method "ssh")

;; Refresh package archives (GNU Elpa)
(unless package-archive-contents
  (package-refresh-contents))

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
  (setq evil-motion-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-emacs-state-cursor  'hbar)
  :ensure t)

(use-package dracula-theme
  :ensure t)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package company
  :config
  (global-company-mode t)
  :bind
  (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(load-theme 'dracula t)
(xterm-mouse-mode t)
(evil-mode 1)
(global-company-mode t)

(require 'eglot)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'json-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'terraform-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(rust-mode "/Users/arturo.fernandez/.cargo/bin/rust-analyzer"))
(add-to-list 'eglot-server-programs '(terraform-mode "~/.local/bin/terraform-eglot.sh"))

(require 'ido)
(ido-mode 1)

;; Only for UI
(when window-system
  (add-to-list 'default-frame-alist '(height . 75))
  (add-to-list 'default-frame-alist '(width . 120))
  (delete-selection-mode 1)
  (copy-face 'italic 'font-lock-comment-face)
  (set-face-foreground 'font-lock-comment-face "#8a877f")
  (set-face-attribute 'default nil :font "JetBrains Mono NL-13")
  (set-frame-font "JetBrains Mono NL-13" nil t))


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil-terminal-cursor-changer evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
