;; Archena's .emacs config file
;; - started 17:44 on 10/05/2009
;; - mostly re-written 20:33 on 30/05/2019
;;
;; http://www.github.com/archena

;; * ----------------
;; * Packages
;; * ----------------

(require 'package)

(setq my-packages
      '(color-theme-sanityinc-tomorrow
        magit
        ;;        exec-path-from-shell
        ;; Programming tooling
	    slime
	    markdown-mode
	    yaml-mode
	    json-mode
        go-mode
        terraform-mode
        jedi
        ;; Data science tooling
        jupyter
        ))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; * ----------------
;; * User interface and keybindings
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
(transient-mark-mode -1)
(global-hl-line-mode t)
;(set-face-background 'hl-line "#333")
(require 'uniquify) ; Uniquify gives buffers sensible unique names

(setq custom-safe-themes t)
(color-theme-sanityinc-tomorrow-night)

;; Things that make life on a Mac better (for when there's no other choice)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil      ; Sensible font
                      :family "Menlo"
                      :height 100
                      :weight 'normal)
  (setq mac-command-modifier 'control)  ; Carbon Emacs seems to swap command and control, which would usually be welcome
  (setq mac-control-modifier 'meta)     ; except I already swap these keys system-wide on a Mac, so they need swapping again for Emacs!
  (setq mouse-wheel-mode nil)
)

;; Keybindings
(global-set-key [f1] 'man)
;;(global-set-key [f6] 'mu4e)
(global-set-key [f8] 'gdb)
(global-set-key [f9] 'compile)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)
(global-set-key "\M-s\M-o" 'multi-occur-in-this-mode)
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)

;; When in GUI mode we don't want C-z to minimise Emacs
(when window-system
  (global-set-key "\C-z" nil)
  (global-set-key "\C-xz" nil))

;; PYIM Chinese input

(require 'pyim)
(require 'pyim-basedict)
(pyim-basedict-enable)
(setq default-input-method "pyim")

;; Web browsing

(setq browse-url-browser-function 'eww-browse-url)

;; * ----------------
;; * Networking
;; * ----------------

(server-start)

;(defun connect-socks ()
;  (interactive)
;  (require 'socks)
;  (setq socks-override-functions 1)
;  (setq socks-noproxy '("localhost"))
;  (setq socks-server (list "Socks server" "localhost" 10010 5)))

;; * ------------------
;; * Programming
;; * ------------------

(setq auto-mode-alist
      (append '(("\\.m$"                  . octave-mode)
                ("\\.ldf$"                . latex-mode)
                ("\\.js$"                 . javascript-mode)
                ("\\.s\\(ml\\|ig\\)\\'"   . sml-mode)
                ("\\.ij[rstp]"            . j-mode)
                ("\\.markdown$"           . markdown-mode)
                ("\\.pom$"                . xml-mode)
                ("\\.\\(asm\\|s\\)$"      . asm-mode)
                ("\\.\\(ino\\|pde\\)$"    . arduino-mode)
                ("\\.\\(bin\\|hex\\|\\out\\|o\\|exe\\|com\\|cmd\\|dll\\|lib\\|dat\\)$" . hexl-mode))
              auto-mode-alist))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode t)

;; Python

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Go
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
  ; Use goimports instead of go-fmt
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

;; cc-mode
(require 'cc-mode)
(c-set-offset 'substatement-open 0) ;; No additional indentation for braces
(c-toggle-auto-newline)

;; ELDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'scheme-mode-hook 'turn-on-eldoc-mode)

;; Scheme
(setq scheme-program-name "guile")

;; SLIME Lisp environment
(setq inferior-lisp-program "sbcl")
(require 'slime-autoloads)
(slime-setup)

;; Haskell mode
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
;(add-to-list 'load-path (my-load-path "scala-mode2"))
;(require 'scala-mode2)

;(defun scala-custom-hook ()
  ;; scala-mode rebinds RET to scala-newline; I wish it didn't.
  ;; I /like/ newline-and-indent
;  (local-set-key "\r" 'newline-and-indent))
;(add-hook 'scala-mode-hook 'scala-custom-hook)

;(defalias 'run-scala 'scala-run-scala)

;(add-to-list 'load-path (my-load-path "ensime/elisp/"))
;(require 'ensime)
;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Prolog
(setq prolog-program "gprolog")

;; Java
(add-hook 'java-mode-hook
      (lambda ()
        "Treat Java 1.5 @-style annotations as comments."
        (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
        (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; LaTeX
(setq tex-dvi-view-command "atril *.pdf")
(setq latex-run-command "pdflatex")

;; CSS
(setq css-indent-offset 2)

;; * ------------------
;; * Org-mode and remember-mode
;; * ------------------

;; Org-mode
(setq org-directory "~/docs/org/")
(setq org-agenda-files '("~/docs/org/tasks.org"))
(define-key global-map "\C-ca" 'org-agenda)

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

(setq org-remember-templates '(("Contact" ?c "** TODO Contact %^{Name} regarding %^{Regarding} :contact:\n" "~/docs/org/tasks.org" "Contact")
                               ("Appointment" ?a "** TODO %^{Name} at %^{Location} %^{Time} :appointment:\n" "~/docs/org/tasks.org" "Appointments")
                               ("Remember" ?r "** TODO %^{Thing}\n" "~/docs/org/tasks.org" "Remember tasks")))

(setq org-tags-match-list-sublevels t)
(setq org-enforce-todo-dependencies t)

;; * ----------------
;; * Mail and IM
;; * ----------------

;(add-to-list 'load-path (my-load-path "mu4e"))
;(require 'mu4e)
;(defalias 'mu 'mu4e)
;(load "email.el")

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

;; Count words in buffer
(defun count-words ()
  (interactive)
  (message "%s" (count-matches "[a-zA-Z\-\']+")))

;; Swap two windows (from Steve Yegge)
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(package-selected-packages
   (quote
    (processing-mode flycheck dash-functional org-trello jedi pomodoro xr pyim pinyin coffee-mode elpy golint go-autocomplete scala-mode dockerfile-mode exec-path-from-shell ein jupyter yaml-mode websocket terraform-mode slime request oauth2 markdown-mode magit json-mode go-mode emojify color-theme-sanityinc-tomorrow circe alert))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
