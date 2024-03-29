;; -*- lexical-binding: t; -*-

;; Package management: straight.el and use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Enable upcase and downcase.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Doom themes and modeline
(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))
(use-package doom-modeline
  :hook
  (window-setup . doom-modeline-mode))
(use-package all-the-icons
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-height 27))

;; Autosaves
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Path and shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))
(setq explicit-bash-args (quote ("--noediting" "--login" "-i")))

;; Dired
(setq insert-directory-program (executable-find "gls"))

;; Remove trailing whitespace
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

;; Smart indenting in visual-line-mode.
(use-package adaptive-wrap)

;; Git
(use-package magit
  :bind ("C-x g" . magit-status))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-;" . mc/mark-next-like-this)
         ("C-'" . mc/mark-previous-like-this)
         ("C-M-;" . mc/mark-all-like-this)))
(use-package multiple-cursors-core
  :straight multiple-cursors
  :bind (:map mc/keymap
         ("<return>" . nil)))

;; Ivy, Counsel, and Swiper
(defun ivy--quote-plus (string)
  "Build a regexp suitable for Ivy from STRING.
Convert whitespace to \".*\" and `regexp-quote' everything else."
  (mapconcat #'regexp-quote (split-string string) ".*"))
(defun ivy--quote-single-space (string)
  "Build a regexp suitable for Ivy from STRING.
Has a similar effect to `ivy--quote-plus', except a single space
is quoted, to allow searching for multi-word phrases."
  (mapconcat #'regexp-quote (split-string string "[\f\t\n\r\v]+\\|  +" t) ".*"))
(defun ivy--quote-ignore-order (string)
  "Build a regexp alist suitable for Ivy from STRING.
Has a similar effect to `ivy--quote-with-space', except the order of
whitespace-separated substrings is ignored."
  (mapcar (lambda (substring)
            (cons (regexp-quote substring) t))
          (split-string string)))
(use-package ivy
  :config
  (ivy-mode)
  (setcdr (assoc t ivy-re-builders-alist) #'ivy--quote-single-space)
  (add-to-list 'ivy-preferred-re-builders (cons #'ivy--quote-plus "quote"))
  (add-to-list 'ivy-preferred-re-builders (cons #'ivy--quote-single-space "quote space"))
  (add-to-list 'ivy-preferred-re-builders (cons #'ivy--quote-ignore-order "quote order")))
(use-package counsel
  :bind (("C-x C-r" . counsel-recentf))
  :config
  (counsel-mode)
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))
(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Company
(use-package company)

;; Flycheck
(use-package flycheck)

;; ;; LSP
(use-package eglot
  :hook ((haskell-mode rustic-mode) . eglot-ensure))
;; (use-package lsp-mode
;;   :straight (lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode" :files (:defaults "clients/*.el"))
;;   :config
;;   (add-hook 'lsp-mode-hook 'flycheck-mode)
;;   (add-hook 'lsp-mode-hook 'company-mode))

;; Spell checking
(defun flycheck-maybe-recheck (_)
  (when (bound-and-true-p flycheck-mode)
   (flycheck-buffer)))
(use-package flycheck-aspell
  :straight (flycheck-aspell :type git :host github :repo "leotaku/flycheck-aspell")
  :config
  (flycheck-aspell-define-checker "text"
    "Text" ("--add-filter" "url")
    (text-mode fundamental-mode))
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'text-aspell-dynamic)
  (advice-add 'ispell-pdict-save :after 'flycheck-maybe-recheck))

;; LaTeX
(use-package tex
  :straight auctex
  :bind (:map TeX-mode-map
         ("\"" . nil))
  :mode (".tex" . LaTeX-mode)
  :config
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  (add-hook 'LaTeX-mode-hook 'flycheck-mode)
  (add-hook 'LaTeX-mode-hook 'adaptive-wrap-prefix-mode))

;; BibTeX
;; For some reason, straight couldn't find bibtex-completion on MELPA, so using manual recipe.
(use-package bibtex-completion
  :straight (bibtex-completion :type git :host github :repo "tmalsburg/helm-bibtex"))
(use-package ivy-bibtex
  :straight (ivy-bibtex :type git :host github :repo "tmalsburg/helm-bibtex")
  :after reftex
  :bind (:map reftex-mode-map
         ("C-c [" . ivy-bibtex-with-local-bibliography))
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-ignore-order)))

;; Haskell
(use-package haskell-mode)

;; Rust
(use-package cargo
  :hook (rustic-mode . cargo-minor-mode))
(use-package rustic)

;; Markdown
(use-package markdown-mode
  :config
  (add-hook 'markdown-mode 'flycheck-mode))

;; Zoom all frames at once
(use-package zoom-frm
  :bind (("C-x C-+" . zoom-frm-in)
         ("C-x C-=" . zoom-frm-in)
         ("C-x C--" . zoom-frm-out)
         ("C-x C-0" . zoom-frm-unzoom)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-parse-self t)
 '(adaptive-fill-regexp "[ 	]*\\(\\\\*[-–!|#%;>*·•‣⁃◦]+[ 	]*\\)*")
 '(bibtex-completion-additional-search-fields '("journal" "booktitle"))
 '(bibtex-completion-bibliography "refs.bib")
 '(bibtex-completion-cite-default-command "citep")
 '(bibtex-completion-cite-prompt-for-optional-arguments nil)
 '(bibtex-completion-display-formats
   '((article . "${author:24} ${title:*} ${year:4} ${journal:24} ${=type=:7}")
     (inproceedings . "${author:24} ${title:*} ${year:4} ${booktitle:24} ${=type=:7}")
     (incollection . "${author:24} ${title:*} ${year:4} ${booktitle:24} ${=type=:7}")
     (inbook . "${author:24} ${title:*} ${year:4} Chapter ${chapter:16} ${=type=:7}")
     (t . "${author:24} ${title:*} ${year:29} ${=type=:7}")))
 '(bibtex-completion-pdf-field "file")
 '(column-number-mode t)
 '(enable-remote-dir-locals t)
 '(exec-path-from-shell-arguments '("-l"))
 '(flycheck-checker-error-threshold 1024)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice "~/")
 '(ispell-silently-savep t)
 '(ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
 '(lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
 '(lsp-rust-analyzer-proc-macro-enable t)
 '(markdown-enable-math t)
 '(neuron-default-zettelkasten-directory "~/Documents/Zettelkasten")
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ns-pop-up-frames nil)
 '(recentf-max-menu-items 200)
 '(recentf-max-saved-items 200)
 '(recentf-mode t)
 '(reftex-cite-format 'natbib)
 '(reftex-label-alist '(("\\begin{restatable}[]{}{*}" nil nil nil 1000 nil)))
 '(reftex-plug-into-AUCTeX t)
 '(reftex-ref-style-alist
   '(("Default" t
      (("\\ref" 13)
       ("\\pageref" 112)))
     ("Varioref" "varioref"
      (("\\vref" 118)
       ("\\vpageref" 103)
       ("\\Vref" 86)
       ("\\Ref" 82)))
     ("Fancyref" "fancyref"
      (("\\fref" 102)
       ("\\Fref" 70)))
     ("Hyperref" "hyperref"
      (("\\autoref" 97)
       ("\\autopageref" 117)))
     ("Cleveref" "cleveref"
      (("\\cref" 13)
       ("\\cref" 32)
       ("\\cref" 99)
       ("\\Cref" 67)
       ("\\cpageref" 100)
       ("\\Cpageref" 68)))))
 '(reftex-ref-style-default-list '("Cleveref"))
 '(reftex-section-prefixes '((0 . "part:") (t . "sec:")))
 '(ring-bell-function 'ignore)
 '(rustic-format-trigger 'on-save)
 '(scroll-bar-mode nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-math-face ((t (:foreground "#67addf"))))
 '(font-latex-sectioning-0-face ((t (:foreground "#fc689d" :weight ultra-bold :height 2.25))))
 '(font-latex-sectioning-1-face ((t (:foreground "#fc689d" :weight semi-bold :height 1.825))))
 '(font-latex-sectioning-2-face ((t (:foreground "#fc689d" :weight semi-bold :height 1.625))))
 '(font-latex-sectioning-3-face ((t (:foreground "#fc689d" :weight semi-bold :height 1.375))))
 '(font-latex-sectioning-4-face ((t (:foreground "#fc689d" :weight semi-bold :height 1.25))))
 '(font-latex-sectioning-5-face ((t (:foreground "#fc689d" :weight semi-bold))))
 '(ivy-modified-buffer ((t (:inherit bold :foreground "#ada296"))))
 '(swiper-match-face-1 ((t (:background "#1B2229" :foreground "#ada296")))))
