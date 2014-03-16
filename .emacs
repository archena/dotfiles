;; Archena's .emacs config file. Started 17:44 on 10/05/2009
;;
;; Much of this file depends on external emacs packages which would need to obtained separately.
;; For this reason, don't simply copy this file to ~/.emacs and expect it to work!
;;
;; http://www.github.com/archena
;;
;; ---------------
;; File contents:
;; ---------------
;; 1) User information
;; 2) Keybindings
;; 3) User interface / system
;; 4) Editing (general)
;; 5) Programming and text
;; 6) Org-mode and remember-mode
;; 7) Mail and communicatin
;; 8) Useful functions
;; 9) Templates
;; ----------------

;; ----------------
;; 1) User settings
;; ----------------

;; Load paths: any downloaded emacs packages go in ~/emacs
(setq my-emacs-base-path "~/emacs")
(add-to-list 'load-path my-emacs-base-path)
(cd "~")

(defun my-load-path (S)
  (concat my-emacs-base-path "/" S))

;; ----------------
;; 2) Keybindings
;; ----------------

;; Programming keybindings
(global-set-key [f1] 'man)
(global-set-key [f6] 'mu4e)
(global-set-key [f8] 'gdb)
(global-set-key [f9] 'compile)

;; Replace buffer-menu with ibuffer
(global-set-key "\C-x\C-b" 'ibuffer)

;; Left, right, up, down window movement
;; n.b. These don't play nicely with org mode!
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; Window resizing
(global-set-key [M-kp-6] 'enlarge-window-horizontally)
(global-set-key [M-kp-4] 'shrink-window-horizontally)
(global-set-key [M-kp-2] 'enlarge-window)
(global-set-key [M-kp-8] 'shrink-window)

;; Editing keybindings
(global-set-key "\r" 'newline-and-indent)

;; Search keybindings
(global-set-key "\M-s\M-o" 'multi-occur-in-this-mode)
(global-set-key "\M-sd" 'show-defuns)

;; Scroll lock
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)

;; Commenting
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)

;; Don't want C-z to minimise when in a windowing system
(when window-system
  (global-set-key "\C-z" nil)
  (global-set-key "\C-xz" nil))

;; * ----------------
;; * User interface / system
;; * ----------------

;; Run as server
(server-start)

;; Socks
(defun connect-socks ()
  (interactive)                         
  (setq socks-override-functions 1)
  (setq socks-noproxy '("localhost"))
  (require 'socks)
  (setq socks-server (list "Socks server" "localhost" 10010 5)))

;; Scroll one line only at the bottom of a window
(setq scroll-step 1)
(setq scroll-conservatively 1)

;; Make compilation window smaller
(setq compilation-window-height 12)

;; Don't ask to save before compiling
(setq compilation-ask-about-save nil)

;; No splash screen
(setq inhibit-splash-screen t)

;; Make 'yes' / 'no' prompts accept 'y' / 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make emacs ask before quitting
(setq confirm-kill-emacs 'y-or-n-p)

;; X clipboard integration
(setq-default x-select-enable-clipboard t)

;; ido-mode
(ido-mode t)

;; uniquify
(require 'uniquify)

;; GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

;; Modeline
(display-time-mode t)
(display-battery-mode t)
(column-number-mode t)

;; Things that make life on a Mac better (for when there's no other choice)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil      ; Sensible font
		      :family "Menlo"
		      :height 100
		      :weight 'normal)
  (setq mac-command-modifier 'control)  ; Carbon Emacs seems to swap command and control, which would usually be welcome
  (setq mac-control-modifier 'meta)     ; except I already swap these keys system-wide on a Mac, so they need swapping again for Emacs!
  (setq mouse-wheel-mode nil)
  ;(defun mac-mwheel-scroll () ())       ; Brute-force way to disable mouse scrolling (the Mac mouse is too sensitive)
  ;(defun mwheel-scroll () ()))
  (setq mac-pass-command-to-system nil) ; On Carbon Emacs useless things happen with this enabled, like C-h hiding the window
)

;; * ------------------
;; * Editing (general)
;; * ------------------

;; Additonal auto-mode regexes
(setq auto-mode-alist (append '(("\\.m$"                  . octave-mode)
                                ("\\.ldf$"                . latex-mode)
                                ("\\.js$"                 . javascript-mode)
                                ("\\.s\\(ml\\|ig\\)\\'"   . sml-mode)
                                ("\\.ij[rstp]"            . j-mode)
                                ("\\.dot\\'"              . graphviz-dot-mode)
                                ("\\.\\(vert\\|frag\\)$"  . glsl-mode)
                                ("\\.cu$"                 . cuda-mode)
                                ("\\.php$"                . php-mode)
                                ("\\.markdown$"           . markdown-mode)
                                ("\\.cs$"                 . csharp-mode)
                                ("\\.fs[iylx]?$"          . fsharp-mode)
                                ("\\.pom$"                . xml-mode)
                                ("\\.yaml$"               . yaml-mode)
                                ("\\.\\(asm\\|s\\)$"      . asm-mode)
                                ("\\.\\(ino\\|pde\\)$"    . arduino-mode)
                                ("\\.\\(bin\\|hex\\|\\out\\|o\\|exe\\|com\\|cmd\\|dll\\|lib\\|dat\\)$" . hexl-mode))
                              auto-mode-alist))

;; Indentation
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; Marking
(transient-mark-mode -1)

;; Highlight line mode
(global-hl-line-mode t)
(set-face-background 'hl-line "#f2f7f2")

;; Enable editing in a jar / tar / zip file
(auto-compression-mode t)

;; Colour theme
(require 'color-theme-tomorrow)
(color-theme-tomorrow--define-theme night)

;; * ------------------
;; * Programming and text
;; * ------------------

;; show-paren-mode
(show-paren-mode t)

;; Multiple cursors
;; (add-to-list 'load-path (my-load-path "multiple-cursors"))
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-x r e") 'mc/edit-lines)

;; Mode-specific scratch buffers
(load "scratch-modes.el")

;; Auto-insert pairs of braces
;;(load "autopair")
;;(autopair-global-mode)

;;(load "auto-indent-mode")
;;(auto-indent-global-mode)

;; Expand region
(add-to-list 'load-path (my-load-path "expand-region"))
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Magit
(add-to-list 'load-path (my-load-path "magit"))
;;(require 'magit)

;; Which function mode
(which-function-mode t)
(setq which-func-modes (append '(python-mode
                                 octave-mode
                                 haskell-mode
                                 sml-mode
                                 j-mode
                                 js-mode
                                 php-mode
                                 glsl-mode
                                 cuda-mode)
                               which-func-modes))

;; Show defuns
(autoload 'show-defuns "show-defuns.el")

;; cc-mode
(require 'cc-mode)
(c-set-offset 'substatement-open 0) ;; No additional indentation for braces
(c-toggle-auto-newline)

;; ELDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'scheme-mode-hook 'turn-on-eldoc-mode)
;; Not using c-eldoc because Semantic-mode provides the same information
;;(autoload 'c-turn-on-eldoc-mode "c-eldoc.el")
;;(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; n.b. Common Lisp (SLIME) and Haskell modes have their own documentation features

;; Semantic / cedet (from bzr source)
(load-file (my-load-path "cedet-bzr/cedet-devel-load.el"))

(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;; ;;(semantic-add-system-include stl_include 'c++-mode)
(global-semantic-idle-local-symbol-highlight-mode)
(global-semantic-idle-completions-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-idle-summary-mode t)

(semantic-mode 1)
(global-ede-mode 1)

;; #####

(defun custom-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  ;;(local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cj" 'semantic-complete-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-c\C-r" 'semantic-symref))
(add-hook 'c-mode-common-hook 'custom-cedet-hook)
(add-hook 'java-mode-hook 'custom-cedet-hook)
(add-hook 'lisp-mode-hook 'custom-cedet-hook)
(add-hook 'lisp-interaction-mode-hook 'custom-cedet-hook)
(add-hook 'scheme-mode-hook 'custom-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'custom-cedet-hook)
(add-hook 'python-mode-hook 'custom-cedet-hook)
(add-hook 'js-mode-hook 'custom-cedet-hook)

(defun c-mode-cedet-hook ()
 (local-set-key "." 'semantic-complete-self-insert)
 (local-set-key ">" 'semantic-complete-self-insert))
(add-hook 'c-mode-common-hook 'c-mode-cedet-hook)

;; ;; JDEE: never got this to work
;; ;; (add-to-list 'load-path (my-load-path "jdee/trunk/jdee/lisp"))
;; ;; (require 'jde)
;; ;; (jde-db-set-source-paths "/usr/lib/jvm/java-6-openjdk")

;; Yasnippet
(require 'yasnippet-bundle)
(setq yas/root-directory "~/emacs/snippets")
(yas/load-directory yas/root-directory)

;; Scheme
(setq scheme-program-name "guile")

;; SLIME Lisp environment
(setq inferior-lisp-program "sbcl")
(require 'slime-autoloads)
(slime-setup)

;; Haskell mode
;; n.b. haskell-site-file contains all the necessary autoloads
(add-to-list 'load-path (my-load-path "haskell-mode"))
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/emacs/haskell-mode/")
(define-key haskell-mode-map (kbd "C-c <") 'haskell-move-nested-left)
(define-key haskell-mode-map (kbd "C-c >") 'haskell-move-nested-right)

(defun haskell-custom-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation))
(add-hook 'haskell-mode-hook 'haskell-custom-hook)

(setq haskell-program-name "ghci")

;; Idris mode
(add-to-list 'load-path (my-load-path "idris-mode"))
(require 'idris-mode)
(defalias 'run-idris 'idris-run)

;; Standard ML mode
(autoload 'sml-mode "sml-mode-4.1/sml-mode-startup.el" "Major mode for standard ML" t)
(autoload 'run-sml "sml-mode-4.1/sml-mode-startup.el" "Run ML REPL" t)
(setq sml-program-name "mosml")

;; Scala
(add-to-list 'load-path (my-load-path "scala-mode2"))
(require 'scala-mode2)

(defun scala-custom-hook ()
  ;; scala-mode rebinds RET to scala-newline; I wish it didn't.
  ;; I /like/ newline-and-indent
  (local-set-key "\r" 'newline-and-indent))
(add-hook 'scala-mode-hook 'scala-custom-hook)

(defalias 'run-scala 'scala-run-scala)

(add-to-list 'load-path (my-load-path "ensime/elisp/"))
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Prolog
(setq prolog-program "gprolog")

;; J-mode
(autoload 'j-mode "j-mode.el" "Major mode for J" t)
(autoload 'j-shell "j-mode.el" "Run J interpreter" t)
(setq j-path "~/src/jgplsrc/j/bin")
(defalias 'run-j 'j-shell)

;; Java
(add-hook 'java-mode-hook
	  (lambda ()
	    "Treat Java 1.5 @-style annotations as comments."
	    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
	    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; Python
(setenv "PYTHONPATH" "~/dev/python")

;; Javascript and ejacs
(add-to-list 'load-path (my-load-path "ejacs"))
(autoload 'js-console "js-console" "ejacs Javascript interpreter" t)

;; JSON
(defun format-json ()
  ;; From http://stackoverflow.com/questions/435847/emacs-mode-to-edit-json/7934783#7934783
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;; .net and mono
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

(defun csharp-custom-hook ()
  ;;(turn-on-auto-revert-mode)
  (setq indent-tabs-mode nil)
  ;;(require 'flymake)
  (flymake-mode -1))
(add-hook  'csharp-mode-hook 'csharp-custom-hook t)

;; FSharp mode
(add-to-list 'load-path (my-load-path "fsharp-mode"))
(autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
(autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)

;; PHP
(autoload 'php-mode "php-mode.el" "Major mode for PHP" t)

;; LaTeX
(setq tex-dvi-view-command "atril *.pdf")
(setq latex-run-command "pdflatex")

;; Yaml
(autoload 'yaml-mode "yaml-mode" "" t)

;; Markdown
(autoload 'markdown-mode "markdown-mode" nil t)

;; Graphviz dot mode
(autoload 'graphviz-dot-mode "graphviz-dot-mode.el" nil t)

;; R and ESS
(defalias 'run-r 'R)

;; GLSL (OpenGL Shader language)
(autoload 'glsl-mode "glsl-mode" nil t)

;; CUDA mode
(autoload 'cuda-mode "cuda-mode" nil t)

;; Arduino mode
(autoload 'arduino-mode "arduino-mode" nil t)

;; Lambda calculus interpreter
(require 'lambdacalc)

;; MongoDB
(add-to-list 'load-path (my-load-path "inf-mongo"))
(require 'inf-mongo)
(defalias 'mongo 'inf-mongo)
(setq inf-mongo-command "/usr/bin/mongo 127.0.0.1:27017")

;; Generic mode
;; n.b. to force Windows modes on: (setq generic-define-mswindows-modes t) before loading generic
;;(require 'generic-x)

;; Smart scan
(require 'smartscan)
(global-smartscan-mode)

(load "pamrel.el")

;; * ------------------
;; * Org-mode and remember-mode
;; * ------------------

;; Org-mode
(setq org-directory "~/docs/org/")
(setq org-agenda-files '("~/docs/org/tasks.org"
                         "~/docs/org/projects.org"
                         "~/docs/org/business.org"
                         "~/docs/org/research.org"
                         "~/docs/org/work.org"))
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

(add-to-list 'load-path (my-load-path "mu4e"))
(require 'mu4e)
(defalias 'mu 'mu4e)
(load "email.el")

;; * ----------------
;; * Themes
;; * ----------------

;; (defun dark-theme ()
;;   (interactive)
;;   (set-background-color "black")
;;   (set-face-background 'default "black")
;;   (set-face-background 'region "black")
;;   (set-face-foreground 'default "white")
;;   (set-face-foreground 'region "gray60")
;;   (set-foreground-color "white")
;;   (set-face-background 'hl-line "#222222")
;;   (set-cursor-color "red"))

;; (defun light-theme ()
;;   (interactive)
;;   (set-background-color "white")
;;   (set-face-background 'default "white")
;;   (set-face-background 'region "lightgoldenrod2")
;;   (set-face-foreground 'default "black")
;;   (set-face-foreground 'region nil)
;;   (set-foreground-color "black")
;;   (set-face-background 'hl-line "#f2f7f2")
;;   (set-cursor-color "black"))

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

;; Insert the date
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

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

;; * -------------------------
;; * Templates
;; * -------------------------

;; I don't actually use these often

;; Insert HTML template
(defun insert-html-template ()
  (interactive)
  (insert (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>'"
                  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"DTD/xhtml1-strict.dtd\">\n"
                  "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n"
                  "<head>\n"
                  "<title></title>\n"
                  "<meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"utf-8\" />\n"
                  "</head>\n"
                  "<body>\n"
                  "\n"
                  "</body>\n"
                  "</html>\n"))
  (indent-buffer))

;; Insert Python self-contained script template
(defun insert-python-template ()
  (interactive)
  (insert (concat "#!/usr/bin/python\n"
                  "\n"
                  "def main():\n"
                  "\t\"\"\" Main program \"\"\"\n"
                  "\n"
                  "if __name__ == '__main__': main()\n")))

;; Insert file comment
(defun insert-file-comment ()
  (interactive)
  (insert (concat "/**\n"
                  " * \n"
                  " * \n"
                  " * Author: \n"
                  " * Date: " (format-time-string "%a %b %e, %Y %l:%M %p") "\n"
                  " */\n")))

;; Insert the 'compile-command' local variable text at the top of buffer
(defun insert-compile-command ()
  (interactive)
  (let ((set-cmd (read-string "Compile command: ")))
    (save-excursion
      (goto-char (point-min))
      (insert (concat "// -*- compile-command:\"" set-cmd "\"; -*-\n")))
    (setq compile-command set-cmd)))
