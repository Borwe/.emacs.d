(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable the toolbar
(tooltip-mode -1)    ; disable tooltips
(set-fringe-mode 10) ; give some breathing room

(menu-bar-mode -1) ; disable the menu bar
(setq visible-bell t) ; setup the visible bell

(column-number-mode)
(menu-bar--display-line-numbers-mode-relative)
(global-display-line-numbers-mode t)

;; use pairing of keys, eg: "" () '' etc...
(electric-pair-mode 1)


(dolist (mode '(org-mode-hook vterm-mode term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; set font
(set-face-attribute 'default nil :font "noto mono" :height 100) 


;; initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;			 ("melpa-stable" . "https://stable.melpa.org/packages")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
;;initialize use-package on non-linux plaforms
;(unless (package-installed-p 'use-package) (package-install 'use-pacakge))
(package-install 'use-package)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package swiper)

(use-package wakatime-mode)
(global-wakatime-mode)

(use-package ivy
	     :diminish
	     :bind (("C-s" . swiper)
		    :map ivy-minibuffer-map
		    ("<tab>" . ivy-alt-done)
		    ("C-l" . ivy-alt-done)
		    ("C-j" . ivy-next-line)
		    ("C-k" . ivy-previous-line)
		    :map ivy-switch-buffer-map
		    ("C-k" . ivy-previous-line)
		    ("C-l" . ivy-done)
		    ("C-d" . ivy-switch-buffer-kill)
		    :map ivy-reverse-i-search-map
		    ("C-k" . ivy-previous-line)
		    ("C-d" . ivy-reverse-i-search-kill))
	     :config
	     (ivy-mode 1))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 3)))

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init (ivy-rich-mode))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable0-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general)
(general-define-key
 "C-M-j" 'counsel-switch-buffer)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-c-u-scroll t)
  (setq evil-want-c-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "M-t") 'vterm)
  (define-key evil-insert-state-map (kbd "C-i") 'completion-at-point)
  (define-key evil-insert-state-map (kbd "C-x f") 'comint-replace-by-expanded-filename)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)

(use-package org)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))


;;vterm
(use-package vterm
  :ensure t)

;; lsp setup
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-modeline-diagnostics-enable t))

(use-package lsp-treemacs
  :after lsp-mode
  :init (lsp-treemacs-sync-mode 1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy)

;;typescript editing
(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :config (setq typescript-indent-level 2))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-treemacs hippie-expand which-key wakatime-mode vterm visual-fill-column use-package typescript-mode rainbow-delimiters magit lsp-ui lsp-ivy ivy-rich helpful general evil-collection doom-themes doom-modeline counsel-projectile company command-log-mode))
 '(wakatime-cli-path "/home/brian/.local/bin/wakatime"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
