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
		  eshell-mode-hook))
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
    "r" 'restart-emacs
    "z" 'writeroom-mode))

(use-package helm
  :config
  (helm-mode t)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)))

(use-package magit)

(use-package mise
  :init
  (global-mise-mode))

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

(defun my/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode)
  (variable-pitch-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :custom
  (org-ellipsis " ▼")
  
  (org-agenda-files "~/Org/tasks.org")
  :config
  (set-face-attribute 'variable-pitch nil
		      :family "Noto Serif"
		      :height 130)
  (set-face-attribute 'fixed-pitch nil
		      :family "Noto Sans Mono"
		      :height 120)

  (set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.4 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.3 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-5 nil :height 1.1 :weight 'bold)
  (set-face-attribute 'org-level-6 nil :height 1.1 :weight 'bold)
  (set-face-attribute 'org-block nil   :inherit 'fixed-pitch :height 1.0)
  (set-face-attribute 'org-code nil    :inherit 'fixed-pitch :height 1.0))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Org/roam/"))
  (org-roam-dailies-directory "daily/")

  (org-roam-dailies-capture-templates
   '(("d" "default" plain
      "* Notes\n%?"
      :if-new (file+head "%<%Y%m%d>.org" "#+title: %<%d.%m.%Y>\n")
      :unnarrowed t)))

  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("b" "book" plain
      (file "~/Org/roam/templates/book.org")
      :target (file+head "pages/<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
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

(use-package org-roam-ui
  :after org-roam
  :hook (org-roam-mode . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package xenops
  :init
  (setq xenops-font-height 100)
  (setq xenops-math-image-scale-factor 0.8)
  (setq xenops-reveal-on-entry t))

(use-package writeroom-mode)
