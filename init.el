;;; package --- bsnux better-defaults + minimum plugins

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

;; evil-mode and changing cursor style for iTerm2
(require 'evil)
(evil-mode 1)
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate) ; or (etcc-on)
  )
(setq evil-motion-state-cursor 'box)  ; █
(setq evil-visual-state-cursor 'box)  ; █
(setq evil-normal-state-cursor 'box)  ; █
(setq evil-insert-state-cursor 'bar)  ; ⎸
(setq evil-emacs-state-cursor  'hbar) ; _

;;(global-hl-line-mode 1)
;;(set-face-attribute hl-line-face nil :underline nil)

(setq tramp-default-method "ssh")

(show-paren-mode t)

;; Installed packages configuration
;; company-mode for auto-completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode 1)
(setq company-dabbrev-downcase nil)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))


(add-to-list 'auto-mode-alist '(".*Dockerfile.*\\'"  . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; flycheck for checking syntax
(require 'flycheck)
(global-flycheck-mode)
(require 'flycheck-yamllint)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))

; Select regions between chars like a boss
(require 'expand-region)
;(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

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
 '(custom-safe-themes
   (quote
    ("11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" default)))
 '(package-selected-packages
   (quote
    (crystal-mode ansible flycheck-yamllint editorconfig evil-terminal-cursor-changer json-mode evil dockerfile-mode molokai-theme atom-one-dark-theme puppet-mode expand-region go-mode groovy-mode flycheck company yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
