;;; better-defaults --- Better defaults for your Emacs

;;; Commentary:

;; M-backspace           Delete previous word
;; M-/                   auto-complete
;; C-/                   undo
;; /ssh:user@host:file   tramp
;; /sudo:file            sudo editing

;;; Code:

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

;;; better-defaults.el ends here
