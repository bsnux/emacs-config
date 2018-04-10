;;; init.el --- Emacs configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;     bsnux Emacs configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:


;; INSTALL PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)


;; BASIC CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
;;(load-theme 'solarized-dark t) ;; load solarized theme
(load-theme 'gruvbox t)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

;; no menu-bar
(menu-bar-mode -1)


(global-linum-mode t) ;; enable line numbers globally

;; Adding spaces between line numbers and buffer content
(add-hook 'linum-before-numbering-hook
   (lambda ()
	 (setq-local linum-format-fmt
	 (let ((w (length (number-to-string
   (count-lines (point-min) (point-max))))))
(concat "%" (number-to-string w) "d")))))

(defun linum-format-func (line)
 (concat
  (propertize (format linum-format-fmt line) 'face 'linum)
  (propertize " " 'face 'mode-line)))
(setq linum-format 'linum-format-func)

;; No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Python
(elpy-enable)

;; No visual bell
(setq visible-bell nil)

;; Making life easier for copy&paste
(cua-mode t)

;; Goto line
(global-set-key (kbd "C-l") 'goto-line)

;; Redefining keymaps for company-mode (used by elpy)
(let ((map company-active-map))
  (define-key map (kbd "\C-n") 'company-select-next)
  (define-key map (kbd "\C-p") 'company-select-previous)
)

;; electric-pair
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

;; Highligthing 80+ lines
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Column number
(setq-default column-number-mode t)

;; evil-mode
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-insert-state-map "\C-a" 'beginning-of-line)
(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
;; evil-mode cursor-change
(unless (display-graphic-p)
        (require 'evil-terminal-cursor-changer)
        (evil-terminal-cursor-changer-activate)
        )
(setq evil-motion-state-cursor 'box)
     (setq evil-visual-state-cursor 'box)
     (setq evil-normal-state-cursor 'box)
     (setq evil-insert-state-cursor 'bar)
     (setq evil-emacs-state-cursor  'hbar)
;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;; refresh a buffere when file changes on disk
(global-auto-revert-mode 1)

;; auto-complete
(require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode t)

;; flycheck
(global-flycheck-mode)

;; ido-mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-n") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "M-3") #'mc/mark-next-like-this)
(global-set-key (kbd "M-4") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)



;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)

;; Neo-tree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

;; no tabs
(setq-default indent-tabs-mode nil)

; If GUI then choose different font
(when window-system
  (tool-bar-mode -1)
  ;(set-face-attribute 'default nil :height 100)
  ;(set-frame-font "Monaco-9.5" t t)
  (set-frame-font "Hack-9.5" t t)
  (exec-path-from-shell-initialize)
  (cua-mode t))

;; auto-complete words
(global-set-key (kbd "C-SPC") 'dabbrev-expand)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
   (require 'go-autocomplete))


(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))


(global-set-key [f7] 'switch-to-buffer)

;;; Packages in `package-selected-packages` can be installed by
;; `package-ins
;; tall-selected-packages` command

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6ac7c0f959f0d7853915012e78ff70150bfbe2a69a1b703c3ac4184f9ae3ae02" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(package-selected-packages
   (quote
    (exec-path-from-shell go-autocomplete solarized-theme editorconfig neotree yaml-mode markdown-mode groovy-mode flycheck auto-complete go-mode go haskell-mode evil-terminal-cursor-changer dockerfile-mode docker gruvbox-theme yasnippet py-yapf color-theme-solarized evil multiple-cursors better-defaults magit elpy material-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
