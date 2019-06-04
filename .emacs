;; Packages.
(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(set-default-font "Menlo 12")

(setq ns-pop-up-frames nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-begin-regexp "begin\\b\\|\\[")
 '(LaTeX-end-regexp "end\\b\\|\\]")
 '(TeX-PDF-mode t)
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(ansi-term-color-vector
   [unspecified "#2d2d2d" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#d3d0c8"])
 '(blink-matching-paren nil)
 '(c-basic-offset 2)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "java"))))
 '(column-enforce-column 79)
 '(column-number-mode t)
 '(comint-process-echoes t)
 '(compilation-message-face (quote default))
 '(coq-prog-args (quote ("-I" "DIR/src")))
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "705f3f6154b4e8fac069849507fd8b660ece013b64a0a31846624ca18d6cf5e1" "0fb6369323495c40b31820ec59167ac4c40773c3b952c264dd8651a3b704f6b5" "4294fa1b78ee65d076a1302f6ed34d42e34f637aae918b7691835adef69bd4cc" "8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "76626efc044daee1c402e50f185bd633d1a688c332bc15c8fd5db4cdf2966b79" "30b7087fdd149a523aa614568dc6bacfab884145f4a67d64c80d6011d4c90837" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "44c5c5862d7b66b010559bf1dfd1406baf6070fea92d4b52f587517b8529928d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" default)))
 '(default-frame-alist (quote ((vertical-scroll-bars))))
 '(enable-remote-dir-locals t)
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(explicit-bash-args (quote ("--noediting" "--login" "-i")))
 '(fci-rule-color "#073642")
 '(fill-column 79)
 '(haskell-indent-offset 2)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(intero-package-version "0.1.32")
 '(js-indent-level 2)
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(org-cycle-separator-lines 1)
 '(package-selected-packages
   (quote
    (adaptive-wrap column-enforce-mode web-mode magit use-package synquid company zoom-frm tuareg sublime-themes solarized-theme sml-mode revive rainbow-delimiters python-mode paredit multiple-cursors monokai-theme mark htmlize haskell-mode exec-path-from-shell auto-complete)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(rainbow-delimiters-max-face-count 5)
 '(reb-re-syntax (quote string))
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#93a1a1")
 '(vc-annotate-color-map
   (quote
    ((20 . "#990A1B")
     (40 . "#FF6E64")
     (60 . "#cb4b16")
     (80 . "#7B6000")
     (100 . "#b58900")
     (120 . "#DEB542")
     (140 . "#546E00")
     (160 . "#859900")
     (180 . "#B4C342")
     (200 . "#3F4D91")
     (220 . "#6c71c4")
     (240 . "#9EA0E5")
     (260 . "#2aa198")
     (280 . "#69CABF")
     (300 . "#00629D")
     (320 . "#268bd2")
     (340 . "#69B7F0")
     (360 . "#d33682"))))
 '(vc-annotate-very-old-color "#93115C")
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 0)
 '(web-mode-sql-indent-offset 2)
 '(web-mode-style-padding 0)
 '(web-mode-tests-directory "~/tests/")
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#D8D8D2" :height 120 :family "Menlo"))))
 '(coq-cheat-face ((t (:background "darkred"))))
 '(coq-solve-tactics-face ((t (:foreground "tomato"))))
 '(font-latex-script-char-face ((t (:inherit font-latex-math-face))))
 '(font-lock-cvariable-face ((t (:inherit (quote font-lock-type-face)))))
 '(proof-eager-annotation-face ((t (:background "#272822"))))
 '(proof-highlight-dependent-face ((t (:background "DarkOrange2"))))
 '(proof-locked-face ((t (:background "#21364A"))))
 '(proof-queue-face ((t (:background "dark magenta"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "#002b36" :foreground "#dc322f" :inverse-video t)))))

;; List of installed packages.
;; (setq use-package-always-ensure t)
;; (use-package tex :ensure auctex)
;; (use-package adaptive-wrap)
;; (use-package auto-complete)
;; (use-package column-enforce-mode)
;; (use-package company)
;; (use-package exec-path-from-shell)
;; (use-package haskell-mode)
;; (use-package htmlize)
;; (use-package intero)
;; (use-package magit)
;; (use-package mark)
;; (use-package monokai-theme)
;; (use-package multiple-cursors)
;; (use-package paredit)
;; (use-package python-mode)
;; (use-package rainbow-delimiters)
;; (use-package revive)
;; (use-package sml-mode)
;; (use-package solarized-theme)
;; (use-package sublime-themes)
;; (use-package synquid)
;; (use-package tuareg)
;; (use-package use-package)
;; (use-package web-mode)
;; (use-package zoom-frm)

;; Put backup files and auto saves in temp.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Focus when we open Emacs from a terminal.
(x-focus-frame nil)

;; Tab stops are 4 spaces apart.
(setq tab-stop-list (number-sequence 4 120 4))

;; Trailing whitespace MUST DIE.
(defun delete-trailing-whitespace-except-current-line ()
  "do delete-trailing-whitespace, except preserve whitespace of current line"
  (interactive)
  (let ((current-line (buffer-substring (line-beginning-position) (line-end-position)))
        (backward (- (line-end-position) (point))))
    (delete-trailing-whitespace)
    (when (not (string-equal (buffer-substring (line-beginning-position) (line-end-position))
                             current-line))
      (delete-region (line-beginning-position) (line-end-position))
      (insert current-line)
      (backward-char backward))))
(add-hook 'before-save-hook 'delete-trailing-whitespace-except-current-line)

;; Zoom all frames at once.
(global-set-key (kbd "C-x C-+") 'zoom-frm-in)
(global-set-key (kbd "C-x C-=") 'zoom-frm-in)
(global-set-key (kbd "C-x C--") 'zoom-frm-out)
(global-set-key (kbd "C-x C-0") 'zoom-frm-unzoom)

;; Change this to make window translucent.
(set-frame-parameter (selected-frame) 'alpha '(100 100))

;; Turn off the bell.
(setq ring-bell-function 'ignore)

;; User files in general
(add-to-list 'load-path "~/.emacs.d/usr/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/usr/")

;; Use same PATH as shell.
(exec-path-from-shell-initialize)

;; Magit.
(global-set-key (kbd "C-x g") 'magit-status)

;; Rainbow parens.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Haskell.
;; (add-hook 'haskell-mode-hook 'intero-mode)

;; Vue.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

;; Proof General.
(load-file "~/.emacs.d/usr/ProofGeneral/generic/proof-site.el")
(eval-after-load "proof-script"
  '(progn
     (define-key proof-mode-map (kbd "C-c RET")
       'proof-goto-point)))

;; Ur/Web.
(let ((urweb-mode-dir "/usr/local/share/emacs/site-lisp/urweb-mode"))
  (if (file-readable-p urweb-mode-dir)
    (progn
      (add-to-list 'load-path urweb-mode-dir)
      (load "urweb-mode-startup"))))

;; Bluespec.
(autoload 'bsv-mode "bsv-mode" "BSV mode" t )
(setq auto-mode-alist (cons  '("\\.bsv\\'" . bsv-mode) auto-mode-alist))
(setq bsv-indent-level 4)

;; Scheme.
(load-file "~/.emacs.d/usr/6.945.el")
(defun xscheme-prompt-for-expression-exit ()
  (interactive)
  (let (
	;; In Emacs 21+, during a minibuffer read the minibuffer
	;; contains the prompt as buffer text and that text is
	;; read only.  So we can no longer assume that (point-min)
	;; is where the user-entered text starts and we must avoid
	;; modifying that prompt text.  The value we want instead
	;; of (point-min) is (minibuffer-prompt-end).
	(point-min (if (fboundp 'minibuffer-prompt-end)
		       (minibuffer-prompt-end)
		     (point-min))))
    (if (eq (xscheme-region-expression-p point-min (point-max)) 'one)
        (exit-minibuffer)
      (error "input must be a single, complete expression"))))

;; SCMUtils.
(defun run-mechanics ()
  (interactive)
  (run-scheme "/usr/local/scmutils/mit-scheme/bin/mit-scheme --library /usr/local/scmutils/mit-scheme/lib --emacs"))

;; Paredit.
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(eval-after-load "paredit"
                 #'(define-key paredit-mode-map (kbd "C-M-i") 'paredit-forward-down))

;; Multiple cursors.
(require 'multiple-cursors)
(global-set-key (kbd "C-;") 'mc/mark-next-like-this)
(global-set-key (kbd "C-'") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-;") 'mc/mark-all-like-this)

;; OCaml
;; Add opam emacs directory to the load-path
(if (executable-find "opam")
  (progn
    (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
    (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
    ;; Load merlin-mode
    (require 'merlin)
    ;; Start merlin on ocaml files
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ; Make company aware of merlin
    (with-eval-after-load 'company
     (add-to-list 'company-backends 'merlin-company-backend))
    ; Enable company on merlin managed buffers
    (add-hook 'merlin-mode-hook 'company-mode)))

;; Kappa.
(require 'kappa)
(add-hook 'kappa-mode-hook
          (lambda () (run-hooks 'prog-mode-hook)))

;; Smtlib
(setq auto-mode-alist (cons '("\\.smt$" . smtlib-mode) auto-mode-alist))
(autoload 'smtlib-mode "smtlib" "Major mode for SMTLIB" t)
(setq smtlib-solver-cmd "z3 -smt2 -in")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Tab-aware line wrapping
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(defun adaptive-wrap-fill-context-prefix (begin end)
  "Like `fill-context-prefix', but with length adjusted by `adaptive-wrap-extra-indent'."
  ;; Note: fill-context-prefix may return nil; See:
  ;; http://article.gmane.org/gmane.emacs.devel/156285
  (let* ((fcp (or (fill-context-prefix begin end) ""))
         (fcp-len (string-width fcp)))
    (if (>= adaptive-wrap-extra-indent 0)
        (concat (make-string fcp-len ?\ )
                (make-string adaptive-wrap-extra-indent ?\ ))
      "")))
