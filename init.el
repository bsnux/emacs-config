;; init.el --- Emacs configuration

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
;(load-theme 'material t) ;; load material theme
(load-theme 'solarized t) ;; load solarized theme
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)


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

;; Multiple-cursors
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "M-3") #'mc/mark-next-like-this)
(global-set-key (kbd "M-4") #'mc/mark-previous-like-this)

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

;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;; refresh a buffere when file changes on disk
(global-auto-revert-mode 1)

;; Packages in `package-selected-packages` can be installed by
;; `package-install-selected-packages` command

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(package-selected-packages
   (quote
    (yasnippet py-yapf color-theme-solarized evil multiple-cursors better-defaults magit elpy material-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
