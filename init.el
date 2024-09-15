(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable the toolbar
(tooltip-mode -1)    ; disable tooltips
(set-fringe-mode 10) ; give some breathing room
(setq-default tab-width 4) ; Set tab width

(menu-bar-mode -1) ; disable the menu bar
(setq visible-bell t) ; setup the visible bell

(require 'saveplace)
(setq-default save-place t) ; Enable saving last location
(save-place-mode)

(defun launch-new-emacs (&optional val)
  "Launch new instance of emacs mostly for debuging"
  (interactive)
  (let ((cmd (if val
				(concat "cd " invocation-directory " && "  invocation-name  " --execute " val )
					  (concat invocation-directory invocation-name)
			   )))
	(shell-command cmd)))

(defun launch-winterm ()
  "Launch emacs instance to test emacs-winterm-mode"
  (interactive)
  (launch-new-emacs "(emacs-winterm)"))

(defun session-save-here ()
  (interactive)
  (if dired-directory
	  (desktop-save dired-directory)
	(error "Please run command inside dired instance")))

(defun session-reload-here ()
  (interactive)
  (progn 
	(if buffer-file-name
		(desktop-change-dir (file-name-directory dir)))
	(if dired-directory
			(desktop-change-dir dired-directory))))

(column-number-mode)
(menu-bar--display-line-numbers-mode-relative)
(global-display-line-numbers-mode t)

;; use pairing of keys, eg: "" () '' etc...
(electric-pair-mode 1)


(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; set font
(set-face-attribute 'default nil :font "Cascadia Mono" :height 130)


;; initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
;;initialize use-package on non-linux plaforms
(unless (package-installed-p 'use-package) (package-install 'use-package))

;;do git submodule init for everything in here
(defun borwe/submodules_init ()
  (message "Starting to git submodule init")
  (shell-command-to-string (concat "cd " (string-replace "\\" "/" (getenv "APPDATA")) "/.emacs.d && "
						 "git submodule update --init --recursive"))
  (message "Done git submodule init"))
(borwe/submodules_init)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package swiper)

(use-package wakatime-mode
  :load-path "lisp/wakatime-mode"
  :config (global-wakatime-mode))

;(use-package emacs-winterm
;  :load-path "lisp/emacs-winterm")

;;LSP installing and configuring
(defun borwe/setup-eglot ()
  "Setup variables and configs for eglot"
  (load-file (concat user-emacs-directory "lsp_installers.el"))
  (with-eval-after-load 'eglot
	(progn
	  (setq eglot-autoshutdown t))))

(use-package eglot
  :config (borwe/setup-eglot))

(use-package cmake-mode)
(use-package cmake-font-lock
  :after cmake-mode)

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
  :custom ((doom-modeline-height 1)))

(use-package doom-themes
  :init (load-theme 'doom-one-light t))

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

(use-package origami
  :ensure t)

(use-package tree-sitter)
(use-package tree-sitter-langs
  :config (global-tree-sitter-mode)
  :after tree-sitter)

(use-package evil
  :after company
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-c-u-scroll t)
  (setq evil-want-c-i-jump nil)
  :config
  (define-key evil-normal-state-map (kbd "<SPC> z") 'eldoc)
  (define-key evil-normal-state-map (kbd "<SPC> k") 'eldoc)
  (define-key evil-normal-state-map (kbd "<SPC> a") 'eglot-code-actions)
  (define-key evil-normal-state-map (kbd "<SPC> f") 'eglot-format)
  (define-key evil-normal-state-map (kbd "<SPC> r") 'eglot-rename)
  (define-key evil-normal-state-map (kbd "<SPC> R") 'eglot-reconnect)
  (define-key evil-normal-state-map (kbd "<SPC> x") 'flymake-show-project-diagnostics)
  (define-key evil-normal-state-map (kbd "M-t") 'vterm)
  (define-key evil-insert-state-map (kbd "<tab>") 'self-insert-command)
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete)
  (define-key evil-insert-state-map (kbd "C-x f") 'comint-replace-by-expanded-filename)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-mode))


(use-package evil-collection
  :after evil
  :config
  (setq evil-want-keybinding nil)
  (evil-collection-init))


(defun borwe/get_cpp_setup_cmd ()
  (cond ((equal system-type 'windows-nt) "vcvars64 && cmake -GNinja -Bbuild -DCMAKE_BUILD_TYPE=Debug")
	((equal system-type 'gnu/linux) "cmake -Bbuild -DCMAKE_BUILD_TYPE=Debug")))
(defun borwe/get_vcpkg_path ()
  (cond ((equal system-type 'windows-nt) "C:/Users/BRIAN/Documents/vcpkg/scripts//buildsystems/vcpkg.cmake")
	((equal system-type 'gnu/linux) "~/Git-Repos/vcpkg/scripts/buildsystems/vcpkg.cmake") ))

(defun borwe/get_cpp_compile_cmd ()
  (concat (borwe/get_cpp_setup_cmd) " -DCMAKE_TOOLCHAIN_FILE=" (borwe/get_vcpkg_path)))

(defun borwe/get_compile_json_cpp_cmd ()
  (concat (borwe/get_cpp_compile_cmd) " -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && "
	  (cond ((equal system-type 'windows-nt) "copy build\\compile_commands.json compile_commands.json")
		((equal system-type 'gnu/linux) "cp build/compile_commands.json compile_commands.json"))))


(defun borwe/cargo_or_trunk_run ()
    (interactive)
    (with-temp-buffer
      (insert-file-contents-literally "Cargo.toml")
      (message (if (search-forward "yew" nil t) "trunk serve --open" "cargo run"))))

(defun borwe/cargo_or_trunk_compile ()
    (interactive)
    (with-temp-buffer
      (insert-file-contents-literally "Cargo.toml")
      (message (if (search-forward "yew" nil t) "trunk serve --open" "cargo run"))))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  (projectile-register-project-type 'cpp-vcpkg-setup '("CMakeLists.txt")
				    :project-file "CMakeLists.txt"
				    :compile  (concat (borwe/get_compile_json_cpp_cmd) " && cmake --build build --config debug"))
  (projectile-register-project-type 'rust-cargo '("Cargo.toml")
				    :project-file "Cargo.toml"
				    :run 'borwe/cargo_or_trunk_run)
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


;; Function top open init file
(defun open-init ()
  "Open init.el file"
  (interactive)
  (find-file user-init-file))

;;Function to handle displaying htop on startup
(defun htop ()
  (interactive)
  (if (eq system-type 'gnu/linux)
    (let ()
	(vterm "htop")
	(vterm-send-string "htop")
	(vterm-send-return))))

;;Function to enable using cmd.exe on winows
(if (eq system-type 'windows-nt)
    (progn (defun cmd-exe ()
	     "Open cmd.exe on windows"
	     (interactive)
	     (let ((explicit-shell-file-name "c:/Windows/System32/cmd.exe"))
	       (shell)))))

;; setup vterm if on linux
(if (eq system-type 'gnu/linux)
    (let ()
	(use-package vterm
	:ensure t)))

;;zig mode
(use-package zig-mode)

;;go mode
(use-package go-mode
  :hook (go-mode . eglot-ensure))

;; Nuru-LSP
(define-derived-mode nuru-mode prog-mode "Nuru Mode")
(setq auto-mode-alist
	  (append '(("\\nr\\'" . nuru-mode)
				("\\sr\\'" . nuru-mode))
			  auto-mode-alist))
(add-hook 'nuru-mode-hook 'eglot-ensure)
;;Add path to nuru on emacs search path
(add-to-list 'exec-path "c:/Users/Brian/Documents/Worspaces/nuru-lsp")
(add-to-list 'eglot-server-programs
			 '(nuru-mode . ("nuru-lsp")))


;; lsp setup
;;(use-package lsp-mode
;;  :after typescript-mode
;;  :after go-mode
;;  :after zig-mode
;;  :after rustic
;;  :after lsp-pyright
;;  :after dart-mode
;;  :commands (lsp lsp-deferred)
;;  :init
;;  (setq lsp-go-gopls-server-path "/home/brian/go/bin/gopls")
;;  (setq lsp-keymap-prefix "C-c l")
;;  (setq lsp-rust-server 'rust-analyzer)
;;  (setq lsp-zig-zls-executable "/home/brian/.vim/plugged/lsp-examples/zig/zls/zig-out/bin/zls")
;;  (setq lsp-modeline-diagnostics-enable t)
;;  :hook
;;  (rust-mode . lsp-deferred)
;;  (c++-mode . lsp-deferred)
;;  (c-mode . lsp-deferred)
;;  (go-mode . lsp-deferred)
;;  (cmake-mode . lsp-deferred)
;; (zig-mode . lsp-deferred)
;;  (dart-mode. lsp-deferred)
;;  (typescript-mode . lsp-deferred))

(use-package rustic)

(use-package yaml-mode)

;;(use-package lsp-dart
  ;;:config
  ;;(setq lsp-dart-flutter-sdk-dir "/home/brian/Apps/flutter"))
  ;(setq lsp-dart-sdk-dir "/home/brian/Apps/flutter/bin/"))
(use-package dart-mode)
  ;;:after lsp-dart)

;;(use-package lsp-treemacs
;;  :after lsp-mode
;;  :init (lsp-treemacs-sync-mode 1))

(use-package company
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (global-company-mode t))

;;typescript editing
(use-package typescript-mode
  :hook
  (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'eglot-server-programs
			   '(typescript-mode . ("C:/Users/Brian/.emacs.d/LSPs/typescript-server/node_modules/.bin/typescript-language-server" "--stdio"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(codeium eldoc-box eglot-box zig-mode yaml-mode which-key wakatime-mode visual-fill-column typescript-mode tree-sitter-langs rustic rainbow-delimiters origami magit lsp-ivy ivy-rich helpful go-mode general evil-collection eglot doom-themes doom-modeline dart-mode counsel-projectile company cmake-font-lock all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
