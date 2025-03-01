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

;; I don't want Emacs to interactively update or persist any configuration; this .emacs file is the sole source of truth
;(setq custom-file (concat user-emacs-directory "/dev/null"))
;(when (file-exists-p custom-file) (load custom-file))

;; Make sure the dictionary is English
(setq ispell-dictionary "en_GB")

;; * ----------------
;; * Packages
;; * ----------------

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; n.b. `use-package-always-ensure` will already install any packages as long as they're mentioned with `use-package`
;; (setq my-packages
;;       '(color-theme-sanityinc-tomorrow
;;         use-package
;;         ;magit
;;         ;;pyim
;;         ;emojify
;;         ;; Programming tooling
;;          markdown-mode
;;          yaml-mode
;;          json-mode
;;         go-mode
;;         go-autocomplete
;;         golint
;;         terraform-mode
;;         ligature
;;         ;pyenv-mode
;;         ;; LSP mode for IDE-style functionality - https://emacs-lsp.github.io
;;         ;lsp-mode
;;         ;lsp-ui
;;         ;company-lsp
;;         ;lsp-python-ms
;;         ;; Emacs ipython notebooks / Jupyter mode
;;         ;;ein
;;         deft
;;         ))
;; (dolist (pkg my-packages)
;;   (unless (package-installed-p pkg)
;;     (package-install pkg)))

(require 'use-package)
(setq use-package-always-ensure t)

;; * ----------------
;; * Fonts
;; * ----------------

;; Using the Fira Code font which needs to be installed separately: https://github.com/tonsky/FiraCode
;; n.b. font size is specific to my machine with 4K UHD displays
;(set-face-attribute 'default nil :font "Fira Code" :height 180)

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
;; * Emacs user interface
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
;;(display-battery-mode t)
(column-number-mode t)
(ido-mode t)
(transient-mark-mode -1)

;; Uniquify gives buffers sensible unique names
(require 'uniquify)

;; Set up the Tomorrow Night theme
(load-theme 'sanityinc-tomorrow-night t)
(global-hl-line-mode t)

;; Web browsing
(setq browse-url-browser-function 'eww-browse-url)

(use-package which-key
  :init (which-key-mode))

;; I don't like Macs, but if there's no other choice, these settings make life a little better
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil      ; Sensible font
                      :family "Menlo"
                      :height 100
                      :weight 'normal)
  ;; On Macs, the 'command' and 'control' keys are the wrong way around, so I always swap them (at a global OS level)
  ;; But I also use Carbon Emacs, which swaps them too. So they need swapping one more time!
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

;; These bindings conflict with org-mode, but I'm too used to them now
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; Useful for programming: after a newline, the cursor will already be at the expected indent level
(global-set-key "\r" 'newline-and-indent)

;; It's especially useful to be able to search all buffers with the current major mode
(global-set-key "\M-s\M-o" 'multi-occur-in-this-mode)

;; Comment according to mode
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)

;; Yes I do use scroll lock
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)

;; Use ibuffer instead of the default buffer listing
(global-set-key "\C-x\C-b" 'ibuffer)

;; * ------------------
;; * Chinese / Mandarin language
;; * ------------------
;;
;; Emacs comes with a set of input methods for a variety of languages (called Mule).
;;
;; Unfortunately the default Chinese input method is barely adequate if you want to actually compose text in Chinese.
;; The best option is to just let the OS provide an input method (such as fcitx on Linux)
;; Alternatively, the PYIM package is a substantial (but imperfect) improvement
;;
;; See also:
;; - https://github.com/tumashu/pyim
;; - https://emacs-china.org

;; (require 'pyim)
;; (require 'pyim-basedict)
;; (pyim-basedict-enable)

;; ;; This overrides M-x toggle-input-method (C-\) to use PYIM instead of Mule.
;; (setq default-input-method "pyim")

;; ;; Fix fonts for org-mode and elsewhere for Chinese characters
;; ;; While this fixes some problems with fonts, it *doesn't* fix org table alignment
;; ;; See https://github.com/chen-chao/zh-align.el
;; (use-package zh-align
;;   :load-path "~/emacs/zh-align.el")
;; (use-package org
;;   :config
;;   (zh-align-set-faces '(org-table)))
;; (setq zh-align-charsets '(han kana cjk-misc))

;; * ------------------
;; * Programming - general
;; * ------------------
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode t)

;; Line numbers on by default (except org-mode)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(with-eval-after-load 'org
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0))))

;; Do follow version-controlled symlinks
(setq vc-follow-symlinks t)

;; * ------------------
;; * Programming - language specific
;; * ------------------
;;
;; We want nice things like auto-complete, code navigation, syntax checking and linting
;; Some languages (e.g. Golang) provide good tools that can integrate easily into Emacs
;; Some languages have good third-party tooling, for instance Scala has Metals
;; For languages including Python and Javascript, the LSP ecosystem looks promising
;;
;; LSP is a protocol to enable programming languages to interoperate with IDEs and text editors.
;; A language 'server' provides the IDE-like functionality for each language, and any IDE/editor can make use of that functionality.
;; (See also Google Grok / Kythe, a simillar but abandonned project https://en.wikipedia.org/wiki/Google_Kythe).
;;
;; See https://emacs-lsp.github.io/lsp-mode
;;     https://microsoft.github.io/language-server-protocol

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config)
;; (setq lsp-ui-sideline-enable nil)
;; (use-package flycheck
;;   :defer t
;;   :hook (lsp-mode . flycheck-mode))

;; (use-package yasnippet
;;   :hook (prog-mode . yas-minor-mode)
;;   :config
;;   (yas-reload-all))

;; ;; No documentation popups by default - use M-x lsp-ui-doc-glance / -show to view docs
;; (setq lsp-ui-doc-enable nil)

;; (use-package lsp-treemacs
;;   :after lsp)

;; ** Python **

;; Pre-requisites: make sure a suitable LSP server is installed, for instance https://github.com/palantir/python-language-server or https://github.com/Microsoft/python-language-server
;(add-hook 'python-mode-hook 'lsp-deferred)
(setq python-shell-interpreter "python3")

;(use-package conda
;  :init
;  (setq conda-env-home-directory "~/anaconda3"))

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

(defun my-go-mode-hook ()
  (require 'golint)
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")

  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

;; ** C and C++ **
;;(require 'cc-mode)
;;(c-set-offset 'substatement-open 0) ;; No additional indentation for braces
;;(c-toggle-auto-newline)

;; ** Lisp and Scheme **
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'scheme-mode-hook 'turn-on-eldoc-mode)

(setq scheme-program-name "guile")

;; Haskell
;; TODO: Check current recommended approach for Haskell
;; n.b. haskell-site-file contains all the necessary autoloads
                                        ;(add-to-list 'load-path (my-load-path "haskell-mode"))
;(require 'haskell-mode-autoloads)
;(add-to-list 'Info-default-directory-list "~/emacs/haskell-mode/")
;(define-key haskell-mode-map (kbd "C-c <") 'haskell-move-nested-left)
;(define-key haskell-mode-map (kbd "C-c >") 'haskell-move-nested-right)

;(defun haskell-custom-hook ()
;  (turn-on-haskell-doc-mode)
;  (turn-on-haskell-indentation))
;(add-hook 'haskell-mode-hook 'haskell-custom-hook)

;(setq haskell-program-name "ghci")

;; Scala
;; TODO look into the current favourite major mode Scala
;; Set up Metals
;(defun scala-custom-hook ()
  ;; scala-mode rebinds RET to scala-newline; I wish it didn't.
  ;; I /like/ newline-and-indent
;  (local-set-key "\r" 'newline-and-indent))
;(add-hook 'scala-mode-hook 'scala-custom-hook)

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

;; Deft (experimental)
(use-package deft)
(setq deft-extensions '("org"))
(setq deft-directory org-directory)
(setq deft-recursive t)

;; (setq remember-annotation-functions '(org-remember-annotation))
;; (setq remember-handler-functions '(org-remember-handler))
;; (add-hook 'remember-mode-hook 'org-remember-apply-template)
;; (define-key global-map "\C-cr" 'org-remember)

;; (setq org-remember-templates '(("Contact" ?c "** TODO Contact %^{Name} regarding %^{Regarding} :contact:\n" "~/docs/org/tasks.org" "Contact")
;;                                ("Appointment" ?a "** TODO %^{Name} at %^{Location} %^{Time} :appointment:\n" "~/docs/org/tasks.org" "Appointments")
;;                                ("Remember" ?r "** TODO %^{Thing}\n" "~/docs/org/tasks.org" "Remember tasks")))


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
