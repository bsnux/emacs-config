;;; init.el -- bsnux Emacs config file

;; Author: Arturo Fernandez

;;; Commentary:
;; Based on https://github.com/dimitri/emacs-kicker

;;; Installation:

;; Install el-get package: https://github.com/dimitri/el-get
;; Add this file to your ~/.emacs.d directory

;;; Code:


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
 (with-current-buffer
	 (url-retrieve-synchronously
	  "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
   (goto-char (point-max))
   (eval-print-last-sexp)))

;; now either el-get is required already, or have been loaded by the
;; el-get installer.

(setq
el-get-sources
'((:name evil
	 :after (progn
	   (define-key evil-normal-state-map "\C-a" 'evil-beginning-of-line)
		   (define-key evil-insert-state-map "\C-a" 'beginning-of-line)
		   (define-key evil-visual-state-map "\C-a" 'evil-beginning-of-line)
		   (define-key evil-motion-state-map "\C-a" 'evil-beginning-of-line)
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
		   (define-key evil-insert-state-map "\C-k" 'evil-delete-line)
		   (define-key evil-visual-state-map "\C-k" 'evil-delete-line)))
  (:name auto-complete
	 :after (progn
		(define-key ac-complete-mode-map "\C-n" 'ac-next)
		(define-key ac-complete-mode-map "\C-p" 'ac-previous)))))

;; packages
(setq
my:el-get-packages
'(el-get
  yasnippet
  auto-complete
  evil
  flycheck
  ecb
  exec-path-from-shell
  magit
 ; editorconfig
  markdown-mode
  yaml-mode
  ace-jump-mode
  dockerfile-mode
  cider
  fiplr
  haskell-mode
  fill-column-indicator
  color-theme
  color-theme-zenburn
  monokai-theme
  color-theme-almost-monokai))

(setq my:el-get-packages
	 (append
	  my:el-get-packages
	  (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

(require 'ecb)
(require 'ecb-autoloads)

;; Yasnippet
(yas-global-mode 1)

(add-to-list 'yas-snippet-dirs "~/.emacs.d/el-get/yasnippet/snippets")

(setq delete-auto-save-files t)     ; Delete unnecesary auto-save files (ex. #%*mail*#')

;; No backup files
(setq make-backup-files nil)

;; Custom init buffer
(setq initial-scratch-message ";; scratch buffer: Lisp evaluation & draft notes")

(line-number-mode 1)                ; have line numbers and
(column-number-mode 1)              ; column numbers in the mode line
(global-hl-line-mode)               ; highlight current line
(set-face-background 'hl-line "#3e4446")
(global-linum-mode 1)               ; add line numbers on the left

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

;; Automatically re-visiting the file in current buffer when it was
;; modified by an external program
(global-auto-revert-mode 1)

;; Show and delete trailing whitespace (on save)
(setq whitespace-style '(lines))
(setq whitespace-line-column 78)
(global-whitespace-mode 1)
(setq-default show-trailing-whitespace t)
(setq default-indicate-empty-lines t)

(evil-mode 1)                           ; evil-mode

;(load-theme 'zenburn t)
(load-theme 'monokai t)

(fset 'yes-or-no-p 'y-or-n-p)           ; Changes all yes/no questions to y/n type

(electric-pair-mode 1)                  ; New in Emacs 24.4


(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-default-buffer-method 'selected-window)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "M-p") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; Saving buffer
;(global-set-key (kbd "C-s") 'save-buffer)

;; have vertical ido completion lists
(setq ido-decorations
	 '("\n-> " "" "\n   " "\n   ..." "[" "]"
   " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;; Disabling auto-save for files
(setq auto-save-default nil)

;; Go to line
(global-set-key (kbd "C-l") 'goto-line)

;; `flycheck` module
;; It works out of the box for many languages but you'll need to install checkers
;; first:
;;      $ pip install flake8
;;      $ npm install jshint -g
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Checking files uisng `flycheck' only when files are saved
(setq flycheck-check-syntax-automatically '(save))


;; Using TRAMP via `'sudo` when trying to edit `root` files
(defadvice ido-find-file (after find-file-sudo activate)
 "Find file as root if necessary."
 (unless (and buffer-file-name
		  (file-writable-p buffer-file-name))
	   (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(when (eq system-type 'darwin)

 ;; default Latin font (e.g. Consolas)
 (set-face-attribute 'default nil :family "Menlo for Powerline")

 ;; default font size (point * 10)
 ;;(set-face-attribute 'default nil :height 115)
 (set-face-attribute 'default nil :height 112)
 ;; (set-face-attribute 'default nil :height 105)
 )

;; Disabling tool-bar
(tool-bar-mode -1)

;; Disabling menu-bar
(menu-bar-mode -1)

;; Make sure environment variables inside Emacs look the same as in the user's shell,
;; becaus OS X GUI applications do not inherit variables from the shell configuration
(when (memq window-system '(mac ns))
 (exec-path-from-shell-initialize))

;; Pretty format for XML. Make sure `xmllint' executable is in your PATH
(defun nxml-pretty-format ()
   (interactive)
   (save-excursion
	   (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
	   (nxml-mode)
	   (indent-region begin end)))

;; Markdown mode config
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Default tab size
(setq default-tab-width 4)
;; Indent for JS
(setq js-indent-level 2)

;; EditorConfig
;;(load "editorconfig")

; Enable mouse support
(unless window-system
 (require 'mouse)
 (xterm-mouse-mode t)
 (global-set-key [mouse-4] (lambda ()
							 (interactive)
							 (scroll-down 1)))
 (global-set-key [mouse-5] (lambda ()
							 (interactive)
							 (scroll-up 1)))
 (defun track-mouse (e))
 (setq mouse-sel-mode t)
)

;; Docker mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Ctags
(defun create-tags (dir-name)
 "Create tags file."
 (interactive "DDirectory: ")
 (shell-command
  (format "ctags -f %s/TAGS -e -R %s -h .py" dir-name (directory-file-name dir-name)))
 )

;; Copy&paste from clipboard on OSX
(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
   (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	 (process-send-string proc text)
	 (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Show Paren mode
(show-paren-mode 1)

;; Cursor type
(set-default 'cursor-type 'bar)

;; Shift the selected region right if distance is postive, left if
;; negative
(defun shift-region (distance)
 (let ((mark (mark)))
   (save-excursion
	 (indent-rigidly (region-beginning) (region-end) distance)
	 (push-mark mark t t)
	 ;; Tell the command loop not to deactivate the mark
	 ;; for transient mark mode
	 (setq deactivate-mark nil))))

(defun shift-right ()
 (interactive)
 (shift-region 1))

(defun shift-left ()
 (interactive)
 (shift-region -1))


(defun run-python-once ()
 (remove-hook 'python-mode-hook 'run-python-once)
 (run-python))

(add-hook 'python-mode-hook 'run-python-once)

;; fiplr mode
(setq fiplr-ignored-globs '((directories (".git" ".svn" "env"))
						   (files ("*.jpg" "*.png" "*.zip" "*~" "*.pyc"))))
(global-set-key (kbd "<f5>") 'fiplr-find-file)

;; fci-mode
(setq-default fci-rule-column 80)
(add-hook 'python-mode-hook 'fci-mode)

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:
(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("13de1e95bbc7475e680e50333e9418becef53cb7f41ab632261efd13f9a4f57d" "a6b3505132c41686521cad3cccdc28ef7cc1f04338073a63146a231a1786537c" default)))
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

