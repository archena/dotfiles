;; Archena's .emacs config file
;; - started 17:44 on 10/05/2009
;; - mostly re-written 20:33 on 30/05/2019
;; - another re-work on 19:41 28/12/2020
;; - customise workflows for org-mode around 04/03/2023
;;
;; http://www.github.com/archena

;; * ------------------
;; * Generic initialisation
;; * ------------------

;; Enable Emacs server
(server-start)

;; This prevents Emacs from updating this config file. It prevents interactive configuration
;(setq custom-file (concat user-emacs-directory "/dev/null"))
;(when (file-exists-p custom-file) (load custom-file))

;; * ----------------
;; * Packages
;; * ----------------

(require 'package)

;; Extra package sources in addition to Elpa
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; use-package auto-installs packages. It's used throughout this file.
(require 'use-package)
(setq use-package-always-ensure t)

;; * ----------------
;; * Fonts
;; * ----------------

;; The Fira Code font just looks nice
;; For installation instructions, see https://github.com/tonsky/FiraCode/wiki/Linux-instructions
;; n.b. the font height is set specifically for my machine with 4K UHD displays
(set-face-attribute 'default nil :font "Fira Code" :height 160)

;; Enable ligatures in programming modes if supported: https://github.com/mickeynp/ligature.el
(unless (version< emacs-version "28.0")
  (use-package ligature)
  (ligature-set-ligatures 'prog-mode '("**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't))

;; * ----------------
;; * User interface
;; * ----------------

(setq inhibit-splash-screen t)
(setq scroll-step 1)
(setq scroll-conservatively 1)
(setq compilation-window-height 12)
(setq compilation-ask-about-save nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default x-select-enable-clipboard t)
(defalias 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(display-time-mode t)
(display-battery-mode t)
(column-number-mode t)
(ido-mode t)
(transient-mark-mode t)

;; Uniquify gives buffers sensible unique names
(require 'uniquify)

;; Set up the Tomorrow Night theme
(use-package color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)
(global-hl-line-mode t)

;; Web browsing
(setq browse-url-browser-function 'eww-browse-url)

;; Displays a popup with possible completions for a key sequence
(use-package which-key
  :init (which-key-mode))

;; I don't like Macs, but if there's no other choice, these settings make life a little better
(when (eq system-type 'darwin)
  ;(set-face-attribute 'default nil      ; Sensible font
  ;                    :family "Menlo"
  ;                    :height 100
  ;                    :weight 'normal)
  ;; On Macs, the 'command' and 'control' keys are the wrong way around, so I always swap them (at a global OS level)
  ;; But I also use Carbon Emacs, which separately swaps them, so they need swapping one more time!
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'meta)
  (setq mouse-wheel-mode nil)
)

;; When in GUI mode we don't want C-z to minimise Emacs
(when window-system
  (global-set-key "\C-z" nil)
  (global-set-key "\C-xz" nil))

;; * ------------------
;; * Key bindings
;; * ------------------

(global-set-key [f1] 'man)
(global-set-key [f8] 'gdb)
(global-set-key [f9] 'compile)

;; Use M-<arrow> keys to move between windows
;; These bindings are overridden org-mode
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; After a newline, indent the next line based on context. Especially valuable in programming.
(global-set-key "\r" 'newline-and-indent)

;; It's especially useful to be able to search all buffers with the current major mode
(global-set-key "\M-s\M-o" 'multi-occur-in-this-mode)

;; Comment a region using the appropriate syntax depending on mode
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)

;; Scroll lock is actually useful sometimes
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)

;; Use ibuffer instead of the default buffer listing
(global-set-key "\C-x\C-b" 'ibuffer)

;; * ------------------
;; * Language
;; * ------------------

;; Make sure the dictionary is British English
(setq ispell-dictionary "en_GB")

;; Chinese language support...
;;
;; Emacs comes with a set of input methods for a variety of languages (called Mule).
;; Unfortunately Mule isn't very good for Chinese character input
;; My preference is to use an IME provided by the OS (such as fcitx on Linux)
;; Alternatively, the PYIM works well - I have it kept here but commented out for reference
;;
;; See also:
;; - https://github.com/tumashu/pyim
;; - https://emacs-china.org

;; (require 'pyim)
;; (require 'pyim-basedict)
;; (pyim-basedict-enable)

;; ;; This overrides M-x toggle-input-method (C-\) to use PYIM instead of Mule.
;; (setq default-input-method "pyim")

;; * ------------------
;; * AI
;; * ------------------
(use-package gptel
  :commands (gptel gptel-send gptel-menu)
  :config

  (setq gptel-model 'mistral-small:latest
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(mistral-small:latest)))

  (define-key gptel-mode-map (kbd "C-<return>") #'gptel-send)
)

(use-package aidermacs
  :commands (aidermacs)
  :config
)

;; * ------------------
;; * Programming - general
;; * ------------------
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode t)

;; Keep line numbers on by default (except org-mode)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(with-eval-after-load 'org
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0))))

;; Automatically follow version-controlled symlinks
(setq vc-follow-symlinks t)

;; * ------------------
;; * Programming - language specific
;; * ------------------

;; Git
(use-package magit
  :ensure t)

;; We all want nice things like auto-complete, code navigation, syntax checking and linting.
;;
;; LSP (Language Server Protocol) enables programming languages to interoperate with IDEs and text editors.
;; Language servers are implemented by programming language vendors. These provide all of the functionality that modern IDEs have, but expose this functionality as an API for an editor to consume.

;; Emacs comes with its own LSP client, eglot
;; n.b. language servers need to be installed separately

(use-package eglot
  :ensure t
  :defer t
  :hook ((python-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives '(("pyright-langserver" "--stdio")
                                          "jedi-language-server"
                                          "pylsp")))))

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode))
  :bind (:map company-active-map
              ("<return>" . nil)
              ("RET" . nil)
              ("C-<return>" . company-complete-selection)
              ([tab] . company-complete-selection)
              ("TAB" . company-complete-selection)))
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; ** Python **
(setq python-shell-interpreter "python3")

;; This came from here: https://mclare.blog/posts/using-uv-in-emacs
(defun uv-activate ()
  "Activate Python environment managed by uv based on current project directory.
Looks for .venv directory in project root and activates the Python interpreter."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (venv-path (expand-file-name ".venv" project-root))
         (python-path (expand-file-name
                       (if (eq system-type 'windows-nt)
                           "Scripts/python.exe"
                         "bin/python")
                       venv-path)))
    (if (file-exists-p python-path)
        (progn
          ;; Set Python interpreter path
          (setq python-shell-interpreter python-path)

          ;; Update exec-path to include the venv's bin directory
          (let ((venv-bin-dir (file-name-directory python-path)))
            (setq exec-path (cons venv-bin-dir
                                  (remove venv-bin-dir exec-path))))

          ;; Update PATH environment variable
          (setenv "PATH" (concat (file-name-directory python-path)
                                 path-separator
                                 (getenv "PATH")))

          ;; Update VIRTUAL_ENV environment variable
          (setenv "VIRTUAL_ENV" venv-path)

          ;; Remove PYTHONHOME if it exists
          (setenv "PYTHONHOME" nil)

          (message "Activated UV Python environment at %s" venv-path))
      (error "No UV Python environment found in %s" project-root))))

(use-package python-black
  :ensure t
  :demand t
  :after python
  :hook ((python-mode . python-black-on-save-mode)))

;; ** Go **

;; Pre-requisites
;; This needs to be accompanied with some Go tooling installation:
;; - Emacs packages: go-mode, go-autocomplete, golint
;; - Go packages:
;;    go get golang.org/x/tools/cmd/...
;;    go get github.com/rogpeppe/godef
;;    go get -u github.com/nsf/gocode
;;    go get golang.org/x/tools/cmd/goimports
;;    go get -u golang.org/x/lint/golint

;; (defun my-go-mode-hook ()
;;   (require 'golint)
;;   ;; Use goimports instead of go-fmt
;;   (setq gofmt-command "goimports")

;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   (if (not (string-match "go" compile-command))
;;       (set (make-local-variable 'compile-command)
;;            "go build -v && go test -v && go vet"))
;;   (local-set-key (kbd "M-.") 'godef-jump)
;;   (local-set-key (kbd "M-*") 'pop-tag-mark))
;; (add-hook 'go-mode-hook 'my-go-mode-hook)

;; ** Lisp and Scheme **
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'scheme-mode-hook 'turn-on-eldoc-mode)

(setq scheme-program-name "guile")

;; ** Prolog **
(setq prolog-program "gprolog")

;; ** CSS **
(setq css-indent-offset 2)

;; * ------------------
;; * Text composition and typesetting
;; * ------------------

;; ** Markdown **
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun dw/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun dw/markdown-mode-hook ()
    (dw/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

;; ** LaTeX **
(setq tex-dvi-view-command "atril *.pdf")
(setq latex-run-command "pdflatex")

;; * ------------------
;; * Org-mode and remember-mode
;; * ------------------

(setq org-directory "~/docs/org/")

;; Include *every* file in org agenda, and always refresh the file list before generating the agenda
(defun refresh-org-agenda-files () (setq org-agenda-files (directory-files-recursively "~/docs/org/" "^[a-zA-Z0-9-]+\\.org$")))
(add-hook 'org-agenda-mode-hook 'refresh-org-agenda-files)

;; Keybindings and preferences
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-tags-match-list-sublevels t)
(setq org-enforce-todo-dependencies t)

;; Convenience functions for using important/urgent tags (Covey / Eisenhower matrix style)
;; See: https://docs.tompurl.com/apps/collectives/p/HqZ2WE93bMSXcdG/Tom%20Purl%27s%20Digital%20Garden/Articles/Tech/Using%20The%20Eisenhower%20Matrix%20In%20Emacs%20Org-Mode?fileId=70582
;; (setq org-tag-alist '(("important" . ?i)
;;                       ("urgent"    . ?u)))
;; (setq org-agenda-custom-commands
;;    '(("1" "Q1" tags-todo "+important+urgent")
;;      ("2" "Q2" tags-todo "+important-urgent")
;;      ("3" "Q3" tags-todo "-important+urgent")
;;      ("4" "Q4" tags-todo "-important-urgent")))

;; TODO states
;; (setq org-todo-keywords
;;       '((sequence "TODO" "DOING" "|" "DONE")))

;; (setq remember-annotation-functions '(org-remember-annotation))
;; (setq remember-handler-functions '(org-remember-handler))
;; (add-hook 'remember-mode-hook 'org-remember-apply-template)
;; (define-key global-map "\C-cr" 'org-remember)

;; (setq org-remember-templates '(("Contact" ?c "** TODO Contact %^{Name} regarding %^{Regarding} :contact:\n" "~/docs/org/tasks.org" "Contact")
;;                                ("Appointment" ?a "** TODO %^{Name} at %^{Location} %^{Time} :appointment:\n" "~/docs/org/tasks.org" "Appointments")
;;                                ("Remember" ?r "** TODO %^{Thing}\n" "~/docs/org/tasks.org" "Remember tasks")))


;; * ----------------
;; * Home automation
;; * ----------------
;; (use-package hass
;;   :ensure t
;;   :init
;;   (setq hass-host "homeassistant.local")
;;   (setq hass-apikey "<...> ")
;;   (setq hass-port 8123)
;;   )

;; * ----------------
;; * Mail and IM
;; * ----------------

;(add-to-list 'load-path (my-load-path "mu4e"))
;(require 'mu4e)
;(defalias 'mu 'mu4e)
;(load "email.el")

;; * ------------------
;; * IRC / chat
;; * ------------------

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

;; * ----------------
;; * Useful functions
;; * ----------------

;; Multi occur in mode, taken from http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/
;; See keybindings section for bindings
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))
(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; Change dos newlines to unix
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; Change unix newlines to dos
(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; Indent the entire buffer
(defun indent-buffer ()
  "Indent entire buffer according to mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (indent-according-to-mode)
      (end-of-line)
      (forward-char 1))))

;; Tabify buffer
(defun tabify-buffer ()
  (interactive)
  (tabify (point-min) (point-max)))

;; Untabify buffer
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(aidermacs color-theme-sanityinc-tomorrow corfu deft emojify gptel
               hass ligature lsp-ui lv magit markdown-mode pyenv-mode
               python-black s spinner twittering-mode which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
