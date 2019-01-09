;; init.el --- Emacs configuration

;; This configuration file has been optimized to be used with XEmacs for
;; macOS

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)


;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
;(load-theme 'material t) ;;
;(load-theme 'gruvbox t) ;;
;(load-theme 'railscasts t)
(load-theme 'atom-one-dark t)
;(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

(global-linum-mode t) ;; enable line numbers globally

(menu-bar-mode 0)

(setq vc-follow-symlinks t)

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

;; Goto line
(global-set-key (kbd "C-l") 'goto-line)

;; Redefining keymaps for company-mode (used by elpy)
(let ((map company-active-map))
  (define-key map (kbd "\C-n") 'company-select-next)
  (define-key map (kbd "\C-p") 'company-select-previous)
)

;; Multiple-cursors
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "M-3") #'mc/mark-next-like-this)
(global-set-key (kbd "M-4") #'mc/mark-previous-like-this)

;; Switching to buffer quickly using Cmd-p
(global-set-key (kbd "s-p") 'switch-to-buffer)

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
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-insert-state-map "\C-a" 'beginning-of-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-k" 'kill-line)

(setq evil-emacs-state-cursor '("SkyBlue2" bar))

;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;; refresh a buffer when file changes on disk
(global-auto-revert-mode 1)

;; editorconfig
(require 'editorconfig)
(editorconfig-mode 1)

;; Packages in `package-selected-packages` can be installed by
;; `package-install-selected-packages` command

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4e5e58e42f6f37920b95a8502f488928b3dab9b6cc03d864e38101ce36ecb968" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(package-selected-packages
   (quote
    (go-mode atom-one-dark-theme markdown-mode editorconfig groovy-mode railscasts-theme gruvbox-theme yasnippet py-yapf color-theme-solarized evil multiple-cursors better-defaults magit elpy material-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282C34" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "nil" :family "Fira Code")))))
