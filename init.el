;;; Package --- init.el
;;;  Configuration file for Emacs running on Mac OS X
;;;


;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;(setq color-theme-is-global t)

;; required by el-get
(setq recipes
      '(el-get
        flycheck
        whitespace
        ido-ubiquitous
        auto-complete
        highlight-parentheses
        yasnippet
        yasnippet-snippets
        multiple-cursors
        darcula-theme
        color-theme-almost-monokai
        fill-column-indicator
        undo-tree
        markdown-mode
        markdown-preview-mode
        evil
        magit
        ecb
        elixir
        cider
        haskell-mode
        color-theme-zenburn
        solarized-emacs
        autopair))
(el-get 'sync recipes)
(el-get 'wait)

(evil-mode 1)

;(require 'color-theme)
;(color-theme-initialize)
;(stq color-theme-is-global t
;;(color-theme-railscasts)
;(require 'darcula-theme)
;(color-theme-almost-monokai)
(load-theme 'zenburn t)
;(load-theme 'solarized-dark t)

(require 'fill-column-indicator)
(setq-default fill-column 80)
(setq fci-rule-use-dashes t)
(setq fci-rule-color "#616161")
(add-hook 'after-change-major-mode-hook 'fci-mode)

(setq initial-scratch-message ";; scratch buffer: Lisp evaluation & draft notes")

;; Undo tree mode
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "<M-z>") 'undo)
(global-set-key (kbd "<M-y>") 'redo)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)

;; Delete unnecesary auto-save files (ex. #%*mail*#')
(setq delete-auto-save-files t)

;; Column and line numbers
(setq-default column-number-mode t)
(setq-default line-number-mode t)
(global-linum-mode 1)

(unless window-system
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                          (let ((w (length (number-to-string
                                            (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(unless window-system
    (setq linum-format 'linum-format-func))

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq standard-indent 4)
(setq default-tab-width 4)

;; Highlight +80 lines
(setq whitespace-style '(lines))
(setq whitespace-style '(empty tabs lines-tail trailing))
(global-whitespace-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)
;; Show and delete trailing whitespace (on save)
(setq-default show-trailing-whitespace t)
(setq default-indicate-empty-lines t)

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )


;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; Server: Open files in server with `emacsclient <filename>`
;;(server-start)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")

;; Soft wrap
(global-visual-line-mode 1)

;; Automatically re-visiting the file in current buffer when it was
;; modified by an external program
(global-auto-revert-mode 1)

;; Ask whether or not to close, and then close if y was pressed
(defun ask-before-closing ()
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to quit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

;; follow symlinks and don't ask
(setq vc-follow-symlinks t)

;; Packages
(autopair-global-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-flake8-maximum-line-length 100)

(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
            ido-max-prospects 10)

;; Load snippets
;; (require 'yasnippet)
;; (setq yas/root-directory (list (concat user-dir (file-name-as-directory "snippets"))
;;                                (concat system-dir (file-name-as-directory "snippets"))
;;                                (concat base-dir (file-name-as-directory "snippets"))
;;                                (concat user-emacs-directory (file-name-as-directory "el-get/yasnippet/snippets"))))

(yas-global-mode 1)

(cua-mode t)

;; Keybindings
(global-set-key "\M-r" 'replace-string)
(global-set-key "\C-l" 'goto-line)
(global-set-key "\M-j" 'join-line)
(global-set-key (kbd "<M-up>") 'beginning-of-buffer)
(global-set-key (kbd "<M-down>") 'end-of-buffer)
(global-set-key "\M-c" 'copy-to-x-clipboard)

;; Font type (X Emacs on Mac OS X);;(set-face-attribute 'default nil
;;                :family "DejaVu Sans Mono for Powerline" :height 135 :weight 'normal)

(set-face-attribute 'default nil
                :family "DejaVu Sans Mono for Powerline" :height 125 :weight 'normal)

;; Let's Emacs X can read env variables from PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" default)))
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

(setq *is-a-mac* (eq system-type 'darwin))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(defun copy-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
      (progn
        (cond
         ((and (display-graphic-p) x-select-enable-clipboard)
          (x-set-selection 'CLIPBOARD (buffer-substring (region-beginning) (region-end))))
         (t (shell-command-on-region (region-beginning) (region-end)
                                     (cond
                                      (*cygwin* "putclip")
                                      (*is-a-mac* "pbcopy")
                                      (*linux* "xsel -ib")))
            ))
        (message "Yanked region to clipboard!")
        (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(defun paste-from-x-clipboard()
  (interactive)
  (cond
   ((and (display-graphic-p) x-select-enable-clipboard)
    (insert (x-get-selection 'CLIPBOARD)))
   (t (shell-command
       (cond
        (*cygwin* "getclip")
        (*is-a-mac* "pbpaste")
        (t "xsel -ob"))
       1))
   ))

(defun my/paste-in-minibuffer ()
  (local-set-key (kbd "M-y") 'paste-from-x-clipboard)
  )

(add-hook 'minibuffer-setup-hook 'my/paste-in-minibuffer)

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 2)
(setq javascript-indent-level 2)

;; Using standard moving keys inside `evil` mode
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
(define-key evil-normal-state-map "\C-d" 'evil-delete-char)
(define-key evil-insert-state-map "\C-d" 'evil-delete-char)
(define-key evil-visual-state-map "\C-d" 'evil-delete-char)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)
;;; init.el ends here
