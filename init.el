;;--------------------------------------------------------
;;         init.el
;;
;;   Configuration file for Emacs running on Mac OS X
;;--------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(setq color-theme-is-global t)

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
        autopair))
(el-get 'sync recipes)
(el-get 'wait)

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-railscasts)

(setq initial-scratch-message ";; scratch buffer: Lisp evaluation & draft notes")

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

;; Server: Open files in server with `emacsclient <filename>`
(server-start)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")

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

;; Show and delete trailing whitespace (on save)
(setq-default show-trailing-whitespace t)
(setq default-indicate-empty-lines t)

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
(global-set-key (kbd "<M-up>") 'beginning-of-buffer)
(global-set-key (kbd "<M-down>") 'end-of-buffer)

;; Font type (X Emacs on Mac OS X);;(set-face-attribute 'default nil
;;                :family "DejaVu Sans Mono for Powerline" :height 135 :weight 'normal)

(set-face-attribute 'default nil
                :family "DejaVu Sans Mono for Powerline" :height 125 :weight 'normal)

;; Let's Emacs X can read env variables from PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
