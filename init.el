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

;; Hide the startup message
(setq inhibit-startup-message t)

;; Color theme
(load-theme 'atom-one-dark t)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

;; Enable line numbers globally
(global-linum-mode t)

(menu-bar-mode 0)

(setq vc-follow-symlinks t)

;; No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Python
(elpy-enable)

;; No visual bell
(setq visible-bell nil)

;; Redefining keymaps for company-mode (used by elpy)
(let ((map company-active-map))
  (define-key map (kbd "\C-n") 'company-select-next)
  (define-key map (kbd "\C-p") 'company-select-previous)
)

;; Multiple-cursors
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "<M-s-down>") #'mc/mark-next-like-this)
(global-set-key (kbd "<M-s-up>") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c ,") 'mc/mark-all-like-this)

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

;; Display a bar for the cursor
(setq-default cursor-type 'bar)

;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;; refresh a buffer when file changes on disk
(global-auto-revert-mode 1)

;; editorconfig
(require 'editorconfig)
(editorconfig-mode 1)

;; Open bigger frame
(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(width . 120))

;; Enabling ido-mode
(require 'ido)
(ido-mode 1)

;; company-mode for auto-completion
(require 'company)
(require 'company-go)
(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode 1)
(add-hook 'go-mode-hook
      (lambda ()
        (set (make-local-variable 'company-backends) '(company-go))
        (company-mode)))

;; Overwrite selected region when pasting code
(delete-selection-mode 1)

;; Select regions between chars like a boss
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Useful shortcuts
(global-set-key (kbd "s-r") 'replace-string)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)
(global-set-key (kbd "s-j") 'join-line)
(global-set-key (kbd "s-\\") 'split-window-right)
(global-set-key (kbd "s-\-") 'split-window-below)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "s-p") 'switch-to-buffer)

;; Golang
(setenv "GOPATH" "/Users/arturofernandez/dev/go")
(add-to-list 'exec-path "/Users/arturofernandez/dev/go/bin")
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Markdown
(setq markdown-command "/usr/local/bin/markdown")

;; `calendar` command will start week on Monday
(setq calendar-week-start-day 1)

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
    (company-go expand-region dockerfile-mode yaml-mode company-shell company-nginx git-gutter+ go-mode atom-one-dark-theme markdown-mode editorconfig groovy-mode railscasts-theme gruvbox-theme yasnippet py-yapf color-theme-solarized evil multiple-cursors better-defaults magit elpy material-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282C34" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "nil" :family "Fira Code")))))
