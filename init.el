;;; init.el ---  better-defaults + useful packages

;;; Commentary:

;; M-backspace           Delete previous word
;; M-/                   auto-complete
;; C-/                   undo
;; /ssh:user@host:file   tramp
;; /sudo:file            sudo editing

;;; Code:
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq inhibit-startup-message t)

(load-theme 'wombat t)

(menu-bar-mode -1)

(setq linum-format "%d ")
(global-linum-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq visible-bell nil)

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode 1)

(require 'ido)
(ido-mode 1)

(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline nil)

(setq tramp-default-method "ssh")

(show-paren-mode t)

;; Installed packages configuration
;; company-mode for auto-completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode 1)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; flycheck for checking syntax
(require 'flycheck)
(global-flycheck-mode)

;; Keybindings
(defun mark-current-word()
  "Mark current word."
  (interactive)
  (backward-word)
  (mark-word)
  )
(global-set-key (kbd "M-d") 'mark-current-word)

(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "ESC <up>") 'beginning-of-buffer)
(global-set-key (kbd "ESC <down>") 'end-of-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (flycheck company yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
