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

(column-number-mode)
(menu-bar--display-line-numbers-mode-relative)
(global-display-line-numbers-mode t)

;; use pairing of keys, eg: "" () '' etc...
(electric-pair-mode 1)


(dolist (mode '(org-mode-hook vterm-mode term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; set font
;;(set-face-attribute 'default nil :font "noto mono" :height 100) 


;; initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
;;initialize use-package on non-linux plaforms
(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package swiper)

(use-package wakatime-mode)
;;(add-to-list 'load-path (concat user-emacs-directory "lisp/wakatime-mode"))
;;(load "wakatime-mode")
(global-wakatime-mode)

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
  :init (load-theme 'doom-dark+ t))

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

(use-package general)
(general-define-key
 "C-M-j" 'counsel-switch-buffer)

(defun borwe/lsp-ui-show-info-scroll ()
  (interactive)
  (lsp-ui-doc-show)
  (run-at-time "1 sec" nil 'lsp-ui-doc-focus-frame))

(use-package evil
  :after lsp-mode
  :after lsp-treemacs
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-c-u-scroll t)
  (setq evil-want-c-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<SPC> z") 'lsp-ui-doc-glance)
  (define-key evil-normal-state-map (kbd "<SPC> n") 'lsp-find-definition)
  (define-key evil-normal-state-map (kbd "<SPC> a") 'lsp-execute-code-action)
  (define-key evil-normal-state-map (kbd "<SPC> d") 'borwe/lsp-ui-show-info-scroll)
  (define-key evil-normal-state-map (kbd "<SPC> h") 'lsp-ui-doc-hide)
  (define-key evil-normal-state-map (kbd "<SPC> x") 'lsp-treemacs-errors-list)
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
	:init (htop)
	:ensure t)))

;; For pyright
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred))))

;;zig mode
(use-package zig-mode)

;; lsp setup
(use-package lsp-mode
  :after typescript-mode
  :after zig-mode
  :after rustic
  :after lsp-pyright
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-zig-zls-executable "/home/brian/.vim/plugged/lsp-examples/zig/zls/zig-out/bin/zls")
  (setq lsp-modeline-diagnostics-enable t)
  :hook
  (rust-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c-mode . lsp-deferred)
  (cmake-mode . lsp-deferred)
  (zig-mode . lsp-deferred)
  (typescript-mode . lsp-deferred))

(use-package rustic)

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
  :config (setq typescript-indent-level 2))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" default))
 '(package-selected-packages
   '(origami vdiff zig-mode lsp-pyright which-key vterm visual-fill-column use-package typescript-mode rustic rainbow-delimiters magit lsp-ui lsp-treemacs lsp-ivy ivy-rich helpful general evil-collection doom-themes doom-modeline counsel-projectile company cmake-font-lock all-the-icons))
 '(warning-suppress-types '((lsp-mode) (lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
