;;; init.el --- Emacs configuration

;; Author: Arturo Fernandez
;; URL: https://github.com/bsnux/emacs-config
;; Keywords: configuration

;;; Commentary:
;; This configuration file has been optimized to be used with XEmacs for
;; macOS 10.x

;;; Code:

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

; No tool-bar. menu-bar still visible
(tool-bar-mode -1)

;; Color theme
(load-theme 'atom-one-dark t)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

;; Enable line numbers globally
(global-linum-mode t)

(setq vc-follow-symlinks t)

;; No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; No visual bell
(setq visible-bell nil)

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
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; Overwrite selected region when pasting code
(delete-selection-mode 1)

;; Select regions between chars like a boss
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Setting ssh as default method for tramp
(setq tramp-default-method "ssh")

;; Useful shortcuts
(global-set-key (kbd "s-r") 'replace-string)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)
(global-set-key (kbd "s-j") 'join-line)
(global-set-key (kbd "s-\\") 'split-window-right)
(global-set-key (kbd "s-\-") 'split-window-below)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "s-p") 'switch-to-buffer)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s-_") 'text-scale-decrease)
(global-set-key (kbd "s-[") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-]") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "s-o") 'ido-find-file)
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "<s-right>") 'next-buffer)
(global-set-key (kbd "<s-left>") 'previous-buffer)

(defun mark-current-word()
  "Mark current word."
  (interactive)
  (backward-word)
  (mark-word)
  )
(global-set-key (kbd "M-d") 'mark-current-word)

(defun newline-without-break-of-line ()
  "Addding new line below."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<s-return>") 'newline-without-break-of-line)

;; Golang
(setenv "GOPATH" "/Users/arturofernandez/dev/go")
(add-to-list 'exec-path "/Users/arturofernandez/dev/go/bin")
(defun my-go-mode-hook ()
  "Hook for go-mode files."
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

;; Asking for confirmation before closing Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Python
(setq python-shell-interpreter "/usr/local/bin/python")
(setq flycheck-flake8rc "/usr/local/bin/flake8")

;; JS
(setq json-reformat:indent-width 2)
(setq js-indent-level 2)

;; flycheck-mode
(require 'flycheck)
(global-flycheck-mode)

;; Ensure env vars inside Emacs look the same as in the user's shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Word-wrapping for org-mode
(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq org-log-done 'time)

;; Rust
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
;; Rust auto-completion provided by `racer' + `company`
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; smart-mode-line
(setq sml/theme 'light)
(setq sml/no-confirm-load-theme t)
(sml/setup)

(defun timestamp ()
  "Insert current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %I:%M%p %Z")))

(defun duplicate-line()
  "Duplicate current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-S-d") 'duplicate-line)

;; Turn on highlighting current line
;;(global-hl-line-mode 1)
;;(set-face-background 'hl-line "#424242")

;; Dash (https://kapeli.com/dash)
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)

;; Ruby
;;   M-x inf-ruby
;;   M-x robe-start
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; Packages in `package-selected-packages` can be installed by
;; `package-install-selected-packages` command

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "4e5e58e42f6f37920b95a8502f488928b3dab9b6cc03d864e38101ce36ecb968" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" default)))
 '(fci-rule-color "#3E4451")
 '(package-selected-packages
   (quote
    (rubocop robe dash-at-point solarized-theme racer smart-mode-line yasnippet-snippets flycheck-rust rust-mode exec-path-from-shell flycheck go-snippets json-mode company-go expand-region dockerfile-mode yaml-mode company-shell company-nginx git-gutter+ go-mode atom-one-dark-theme markdown-mode editorconfig groovy-mode railscasts-theme gruvbox-theme yasnippet py-yapf evil multiple-cursors better-defaults magit material-theme)))
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282C34" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "nil" :family "Fira Code")))))
