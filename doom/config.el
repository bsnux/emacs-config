;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Arturo Fernandez"
      user-mail-address "@")

(setq doom-font (font-spec :family "Hack" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'base16-ocean)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

;; Open bigger frame
(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(width . 120))

;; Beginning and end of buffer the macOS way
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)

;; No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; :vs
(global-set-key (kbd "s-\\") 'split-window-right)

;; Open project file
(global-set-key (kbd "s-p") 'helm-projectile-find-file)

;; Style for window divider
(set-face-foreground 'vertical-border (doom-color 'red))

;; Word wrapping
(global-visual-line-mode 1)

; Multiple cursors
(global-set-key (kbd "s-d") 'evil-multiedit-match-symbol-and-next)
