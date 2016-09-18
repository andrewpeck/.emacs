(setq package-enable-at-startup nil) (package-initialize)
(setq org-startup-indented t)

;; turns off signature check when installing packages.. should usually be off, but useful sometimes
(setq package-check-signature nil) 

;; remember undo history
(setq undo-tree-auto-save-history t)

;; Set encoding
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Clipboard
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq x-select-enable-clipboard t)

;; Syntax highlighting in org-mode
(setq org-src-fontify-natively t)

;; hide markup in org-mode 
;; (setq org-hide-emphasis-markers t)


; list the packages you want
(setq package-list '(evil evil-tabs evil-numbers))

;; marmalade archive
(require 'package)
(setq package-archives '(
                        ;; ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/"))
)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
          (package-install package)))



;; 
(modify-frame-parameters nil '((wait-for-wm . nil)))
(setq org-agenda-inhibit-startup nil)

;; ;; latex stuff
;; (if (eq system-type 'windows-nt)
;;     (progn
;; 
;;     )
;;     (progn
;; 	    
;; 	    ;; window decoration
;; 		(if window-system (require 'font-latex))
;; 		(setq font-lock-maximum-decoration t)
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
(add-to-list 'load-path "~/.emacs.d/verilog")
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode))

;; evil mode tabs
(global-evil-tabs-mode t)

; evil mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

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

;; numbers
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)


;; line numbering
(defvar my-linum-current-line-number 0)

(setq linum-format 'my-linum-relative-line-numbers)

(defun my-linum-relative-line-numbers (line-number)
  (let ((test2 (1+ (- line-number my-linum-current-line-number))))
    (propertize
      (number-to-string (cond ((<= test2 0) (1- test2))
                              ((> test2 0) test2)))
      'face 'linum)))

(defadvice linum-update (around my-linum-update)
           (let ((my-linum-current-line-number (line-number-at-pos)))
             ad-do-it))
(ad-activate 'linum-update)

(require 'epa-file)
(epa-file-enable)


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

(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

;; options
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "pdflatex")
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(ansi-color-names-vector ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (whiteboard)))
 '(custom-safe-themes (quote ("badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors (--map (solarized-color-blend it "#fdf6e3" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#c85d17") (60 . "#be730b") (80 . "#b58900") (100 . "#a58e00") (120 . "#9d9100") (140 . "#959300") (160 . "#8d9600") (180 . "#859900") (200 . "#669b32") (220 . "#579d4c") (240 . "#489e65") (260 . "#399f7e") (280 . "#2aa198") (300 . "#2898af") (320 . "#2793ba") (340 . "#268fc6") (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(weechat-color-list (quote (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight semi-bold :height 100 :width normal)))))
