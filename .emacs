(setq package-enable-at-startup nil) (package-initialize)
(setq org-startup-indented t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; turns off signature check when installing packages.. should usually be off, but useful sometimes
(setq package-check-signature nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository

; list the packages you want
;; (setq package-list
;;    '(
;;      use-package
;;     )
;; )


; init
(package-initialize)

;; (unless package-archive-contents
;;     (package-refresh-contents))
;;
;; (dolist (package package-list)
;;     (unless (package-installed-p package)
;;           (package-install package)))

(use-package adaptive-wrap

  :ensure t

  :config

  (setq-default adaptive-wrap-extra-indent 2)
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  (global-visual-line-mode +1)


; (when (fboundp 'adaptive-wrap-prefix-mode)
;  (defun my-activate-adaptive-wrap-prefix-mode ()
;        "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
;    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1))
;    )
; )
)

(use-package exec-path-from-shell :ensure t)
(use-package projectile :ensure t)
(use-package helm-projectile :ensure t)
(use-package helm-ag :ensure t)
(use-package magit :ensure t)
(use-package auto-complete
             :ensure t
;             :config
;             (use-package auto-complete-verilog :ensure t)
)
(use-package helm :ensure t)

; vimrc mode
(use-package vimrc-mode
     :ensure t
     :config
     (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
)

(setq vhdl-basic-offset 2)




; org mode
(setq org-startup-folded 'f)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save history mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq savehist-additional-variables    ;; also save...
  '(search-ring regexp-search-ring)    ;; ... my search entries
  savehist-file "~/.emacs.d/savehist") ;; keep my home clean
(savehist-mode t)                      ;; do customization before activate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remember undo history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq undo-tree-auto-save-history t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown mmm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

(defun my-mmm-markdown-auto-class (lang &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^```" lang "[\n\r]+"))
        (back "^```"))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))

;; Mode names that derive directly from the language name
(mapc 'my-mmm-markdown-auto-class
      '("verilog" "vhdl" "awk" "bibtex" "c" "cpp" "css" "html" "latex"
        "lisp" "makefile" "markdown" "python" "r" "ruby" "sql" "stata" "xml"))

(setq mmm-parse-when-idle 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting in org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-src-fontify-natively t)

;; hide markup in org-mode
;; (setq org-hide-emphasis-markers t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  what does this do?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(modify-frame-parameters nil '((wait-for-wm . nil)))
(setq org-agenda-inhibit-startup nil)

;; ;; latex stuff
;; (if (eq system-type 'windows-nt)
;;     (progn
;;
;;     )
;;     (progn
;;
;; ;; window decoration
;; (if window-system (require 'font-latex))
;; (setq font-lock-maximum-decoration t)
;;
;;         (load "auctex.el" nil t t)
;;         (load "preview-latex.el" nil t t)
;;         (add-hook 'message-mode-hook 'turn-on-orgtbl)
;;         (setq TeX-PDF-mode t)                ; PDF mode (rather than DVI-mode)
;;         (setq TeX-view-program-list '(("Atril" "atril --page-index=%(outpage) %o")))
;;         (setq TeX-view-program-selection '((output-pdf "atril")))
;;      )
;; )


;; default browser
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "google-chrome")

;; turn off toolbar
(tool-bar-mode -1)

;; default tab width
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)


;; turn on verilog mode
; (add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; evil mode
(use-package evil
    :ensure t
    :init

    :config

    (evil-mode 1)

    (setq-default
      evil-search-module 'evil-search
      evil-shift-width 2
    )

    ;; turn off evil bindings in these modes
    (dolist (mode '(
        ag-mode
        flycheck-error-list-mode
        git-rebase-mode
    ))
    (add-to-list 'evil-emacs-state-modes mode))


  ; (evil-add-hjkl-bindings package-menu-mode-map 'emacs
  ;     ((kbd "/") 'evil-search-forward)
  ;     ((kbd "n") 'evil-search-next)
  ;     ((kbd "N") 'evil-search-previous)
  ; )
;     "Set up `evil' bindings for `package-menu'."

    (evil-add-hjkl-bindings package-menu-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous)

     ;; option for enabling vi keys in the minibuffer
     ;; Addresses evil-core.el:163 TODO

     (mapcar (lambda (keymap)
	       (evil-define-key 'insert (eval keymap) [return] 'exit-minibuffer)
	       (evil-define-key 'normal (eval keymap) [return] 'exit-minibuffer)
)
             ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/
	     ;; Text-from-Minibuffer.html#Definition of minibuffer-local-map
	     '(minibuffer-local-map
	       minibuffer-local-ns-map
	       minibuffer-local-completion-map
	       minibuffer-local-must-match-map
	       minibuffer-local-isearch-map))

     (add-hook 'minibuffer-setup-hook
     	       '(lambda ()
     		  (set (make-local-variable 'evil-echo-state) nil)
		  ;; (evil-set-initial-state 'mode 'insert) is the evil-proper
		  ;; way to do this, but the minibuffer doesn't have a mode.
		  ;; The alternative is to create a minibuffer mode (here), but
		  ;; then it may conflict with other packages' if they do the same.
		  (evil-insert 1)))



  ;; https://github.com/jojojames/evil-collection
  ;(;defun evil-collection-package-menu-setup ()
   ; (evil-define-key 'motion package-menu-mode-map
   ;   "i" 'package-menu-mark-install
   ;   "U" 'package-menu-mark-upgrades
   ;   "d" 'package-menu-mark-delete

   ;   ;; undo
   ;   "u" 'package-menu-mark-unmark

   ;   ;; execute
   ;   "x" 'package-menu-execute

   ;   ;; "q" 'quit-window ; macros can make sense here.
   ;   "ZQ" 'evil-quit
   ;   "ZZ" 'quit-window
   ;   )
  ;;(evil-set-initial-state 'package-menu-mode-map 'motion)
   ;)


  ;(with-eval-after-load 'package-menu (evil-collection-package-menu-setup))
  (evil-set-initial-state 'package-menu-mode 'normal)




    (use-package evil-leader
        :ensure t
        :config
        (global-evil-leader-mode)
    )

    (use-package evil-tabs
        :ensure t
        :config
        (global-evil-tabs-mode t)
    )

    (use-package evil-numbers
        :ensure t
        :config
        ;; can't use standard vim keybindings because it conflicts with emacs
        (define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/inc-at-pt)
        (define-key evil-normal-state-map (kbd "C-S-x") 'evil-numbers/dec-at-pt)
    )

    (use-package evil-magit
        :ensure t
        :config
        (evil-leader/set-key "g" 'magit-status)
    )

    (use-package evil-surround
        :ensure t
        :config
        (global-evil-surround-mode 1)
    )
)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)


;; Align with spaces only

(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

; recent files with \f
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(run-at-time nil (* 5 60) 'recentf-save-list)
(evil-leader/set-key "rr" 'helm-recentf)
(evil-leader/set-key "rtw" 'delete-trailing-whitespace)
(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "c" 'helm-projectile-ag)
(evil-leader/set-key "rtw" 'delete-trailing-whitespace)
(setq helm-locate-fuzzy-match nil
          helm-locate-command "mdfind -interpret -name %s %s")
(evil-leader/set-key "f" 'helm-locate)

;; center screen on search result

(defun my-center-line (&rest _)
  (evil-scroll-line-to-center nil))
(advice-add 'evil-search-next :after #'my-center-line)

(local-set-key [tab] 'tab-to-tab-stop)

(with-eval-after-load 'evil-maps
  (bind-key* "C-M-t" 'elscreen-create)
  (bind-key* "M-t"    'elscreen-create)
  (bind-key* "C-<tab>" 'elscreen-next)
  (bind-key* "C-S-<tab>" 'elscreen-previous)
)

;; Overload shifts so that they don't lose the selection
;; c.f. https://superuser.com/questions/684540/evil-mode-evil-shift-left-loses-selection

(define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)
(define-key evil-visual-state-map [tab] 'djoyner/evil-shift-right-visual)
(define-key evil-visual-state-map [S-tab] 'djoyner/evil-shift-left-visual)

(defun djoyner/evil-shift-left-visual ()
  (interactive)
  (call-interactively 'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(defun djoyner/evil-shift-right-visual ()
  (interactive)
  (call-interactively 'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(projectile-global-mode)

(with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map "\C-p" nil)
    (bind-key* "C-p" 'helm-projectile-find-file)
)

(evil-leader/set-key "tag" 'projectile-regenerate-tags)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-frame-font "Monaco 12" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; text mode in variable pitch
;; (add-hook 'text-mode-hook 'variable-pitch-mode)

;; org mode hooks
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'visual-line-mode)

;; different fonts for nt / linux
;; (if (eq system-type 'windows-nt)
;;     (progn
;;         (set-face-attribute 'variable-pitch nil :family "Consolas")
;;         (set-face-attribute 'variable-pitch nil :height 90)
;;
;;         (set-face-attribute 'fixed-pitch nil    :family "Courier New")
;;         (set-face-attribute 'fixed-pitch nil    :height  90)
;;     )
;;     (progn
;;         (set-face-attribute 'variable-pitch nil :family "DejaVu Sans Mono")
;;         (set-face-attribute 'variable-pitch nil :height 100)
;;
;;         (set-face-attribute 'fixed-pitch nil    :family "Courier STD")
;;         (set-face-attribute 'fixed-pitch nil    :height  100)
;;     )
;; )

;; different fonts for org mode tables, code, blocks vs. other text
;; (defun my-adjoin-to-list-or-symbol (element list-or-symbol)
;;   (let ((list (if (not (listp list-or-symbol))
;;                   (list list-or-symbol)
;;                 list-or-symbol)))
;;     (require 'cl-lib)
;;     (cl-adjoin element list)))
;;
;; (eval-after-load "org"
;;   '(mapc
;;     (lambda (face)
;;       (set-face-attribute
;;        face nil
;;        :inherit
;;        (my-adjoin-to-list-or-symbol
;;         'fixed-pitch
;;         (face-attribute face :inherit))))
;;     (list 'org-code 'org-block 'org-table 'org-block-background)))

;; display inline images at startup
(setq org-startup-with-inline-images t)

;; revert cuffer with C-c r
(global-set-key (kbd "C-c r") (lambda ()
                                (interactive)
                                (revert-buffer t t t)
                                (message "buffer is reverted")))

;; turn on flyspell mode by default
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun org-export-as-pdf-and-open ()
  (interactive)
  (save-buffer)
  (org-open-file (org-latex-export-to-pdf)))

(add-hook
 'org-mode-hook
 (lambda()
   (define-key org-mode-map
       (kbd "<f5>") 'org-export-as-pdf-and-open)))

;; options
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "pdflatex")
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (liso)))
 '(custom-safe-themes
   (quote
    ("3448e3f5d01b39ce75962328a5310438e4a19e76e4b691c21c8e04ca318a5f62" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "4156d0da4d9b715c6f7244be34f2622716fb563d185b6facedca2c0985751334" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(markdown-command "multimarkdown")
 '(markdown-header-scaling t)
 '(package-selected-packages
   (quote
    (helm-projectile helm-ag evil-magit auto-complete-auctex auto-complete-clang-async auto-complete-verilog auto-complete helm vimrc-mode evil-leader github-modern-theme liso-theme flatui-dark-theme projectile flatui-theme leuven-theme exec-path-from-shell markdown-mode+ markdown-mode evil-tabs evil-numbers colemak-evil bind-key 0blayout)))
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(add-to-list 'load-path "~/.emacs.d/lisp/")
; (load "ucf-mode")



(require 'epa-file)
(epa-file-enable)

; features to add
; 1)  linter
; 2)  magit
; 3)  add root libs
; 4)  root th1f etc macros
; 5)  line-wrapping
; 6)  magic increment
; 7)  splitbelow, splitright
; 8)  insert gates on .h .hpp
; 9)  insert vhdl boilerplate
; 10) tab completion
; 11) comment block inserter
; 12) latex compilaton
; 13) folding
; 14) undotree
; 15) easily move lines around
; 16) tabularize

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; autocomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ac-config-default)
(global-auto-complete-mode t)


; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; use ido to open files
(define-key evil-ex-map "e " 'ido-find-file)

(evil-leader/set-key "b" 'helm-mini)

;; dired
(evil-leader/set-key "E" 'dired)

;; helm

; from: http://cachestocaches.com/2016/12/vim-within-emacs-anecdotal-guide/
; (define-key helm-map (kbd "C-j") 'helm-next-line)
; (define-key helm-map (kbd "C-k") 'helm-previous-line)
; (define-key helm-map (kbd "C-h") 'helm-next-source)
; (define-key helm-map (kbd "C-S-h") 'describe-key)
; (define-key helm-map (kbd "C-l") (kbd "RET"))
; (define-key helm-map [escape] 'helm-keyboard-quit)
; (dolist
;     (keymap (list helm-find-files-map helm-read-file-map))
;     (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
;     (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
;     (define-key keymap (kbd "C-S-h") 'describe-key)
; )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-linum-mode 1)



; treat underscore as part of a word to match vim behavior
(modify-syntax-entry ?_ "w")
