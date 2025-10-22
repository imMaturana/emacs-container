;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; Initialize Emacs' builtin use-package
;; (is there really a need to use an external package manager?)
(require 'use-package)
(setq use-package-always-ensure t) ; Ensure all packages are loaded


(use-package emacs
  :custom
  (inhibit-startup-message t) ; Disable startup message
  (ring-bell-function 'ignore) ; Disable the annoying bell

  ;; Set up a file for custom variables
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  ;; Disable ui features
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Display column and line number
  (column-number-mode)
  (global-display-line-numbers-mode)

  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
		  term-mode-hook
		  eshell-mode-hook
		  eww-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  
  ;; Set font
  (set-face-attribute 'default nil
		      :font "Hack Nerd Font Mono"
		      :height 120)
  
  ;; Enable Which Key
  (which-key-mode)

  ;; Enable global completion preview
  (global-completion-preview-mode)
  (fido-mode t)

  ;; UTF-8
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8)
  :bind (("C-M-j" . next-buffer)
	 ("C-M-k" . previous-buffer)

	 ;; Quick toggle between buffers
	 ("C-," . (lambda () (interactive) (switch-to-buffer nil)))

	 ;; Use ESC to quit prompts
	 ("<escape>" . keyboard-escape-quit)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (display-line-numbers-type 'relative)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my/leader-keys
    "SPC" 'eshell
    "e r" 'restart-emacs
    "z" 'writeroom-mode
    "d" 'dashboard-open))

(use-package helm
  :init
  (helm-mode t)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)))

(use-package projectile
  :init
  (setq projectile-auto-discover t)
  (setq projectile-project-search-path '("~/Projects"))
  (setq projectile-cleanup-known-projects nil)
  :config
  (projectile-discover-projects-in-search-path)
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package helm-projectile
  :after '(helm projectile)
  :init
  (helm-projectile-on))

(use-package magit)

(use-package mise
  :init
  (global-mise-mode))

(use-package emacs ; tree-sitter
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	  (heex "https://github.com/phoenixframework/tree-sitter-heex"))))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-bar-width 5)
  (doom-modeline-icon t))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (doom-themes-org-config) ; Improves org-mode's fontification
  (load-theme 'doom-one t))

(use-package org
  :preface
  (setq my/org-font-height 130)

  (defun my/org-mode-setup ()
    (visual-line-mode)
    (variable-pitch-mode 1)
    (setq evil-auto-indent nil))
  :hook (org-mode . my/org-mode-setup)
  :custom
  (org-indent-mode)
  (org-ellipsis "…")
  
  (org-agenda-files "~/Org/tasks.org")
  :config
  (set-face-attribute 'org-document-title nil :height 1.5)
  (set-face-attribute 'org-level-1 nil :height 1.0)
  (set-face-attribute 'org-level-2 nil :height 1.0)
  (set-face-attribute 'org-level-3 nil :height 1.0)
  (set-face-attribute 'org-level-4 nil :height 1.0)
  (set-face-attribute 'org-level-5 nil :height 1.0)
  (set-face-attribute 'org-level-6 nil :height 1.0)
  (set-face-attribute 'org-block nil   :inherit 'fixed-pitch :height 1.0)
  (set-face-attribute 'org-code nil    :inherit 'fixed-pitch :height 1.0)
  :bind (:map org-mode-map
	      ("C-c l" . org-toggle-link-display)))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Roam/"))
  (org-roam-dailies-directory "daily/")

  (org-roam-dailies-capture-templates
   '(("d" "default" plain
      "* Notes\n%?"
      :if-new (file+head "%<%Y%m%d>.org" "#+title: %<%d.%m.%Y>\n")
      :unnarrowed t)))

  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "pages/%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("b" "book" plain
      (file "~/Roam/templates/Book.org")
      :target (file+head "pages/%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  (require 'org-roam-dailies)
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n c" . org-roam-dailies-capture-today)
	 (:map org-mode-map
	       ("C-c n i" . org-roam-node-insert))
	 (:map org-roam-dailies-map
	       ("y" . org-roam-dailies-capture-yesterday)
	       ("t" . org-roam-dailies-capture-tomorrow)))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map))

(use-package xenops
  :after org
  :init
  (setq xenops-font-height (/ my/org-font-height 2))
  (setq xenops-math-image-scale-factor 0.7)
  (setq xenops-reveal-on-entry t))

(use-package writeroom-mode)

(use-package nerd-icons)

(use-package nerd-icons-dired
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dashboard
  :hook (after-init . dashboard-refresh-buffer)
  :init
  (setq dashboard-startup-banner 'logo)

  (when (package-installed-p 'nerd-icons)
    (setq dashboard-display-icons-p t)
    (setq dashboard-icons-type 'nerd-icons)

    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t))

  (when (package-installed-p 'projectile)
    (setq dashboard-projects-backend 'projectile)
    (require 'projectile))
  :custom
  (dashboard-startupify-list '(dashboard-insert-banner
			       dashboard-insert-newline
			       dashboard-insert-banner-title
			       dashboard-insert-newline
			       dashboard-insert-init-info
			       dashboard-insert-newline
			       dashboard-insert-navigator
			       dashboard-insert-newline
			       dashboard-insert-items
			       dashboard-insert-newline
			       dashboard-insert-footer))

  (dashboard-items '((bookmarks . 5)
		     (projects  . 5)
		     (recents   . 5)))

  (dashboard-item-shortcuts '((bookmarks . "m")
			      (projects  . "p")
			      (recents   . "r")))
  :config
  (dashboard-setup-startup-hook))
