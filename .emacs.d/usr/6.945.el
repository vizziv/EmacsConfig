;; Use xscheme, not cmuscheme.
(require 'xscheme)

(add-to-list 'load-path "~/.emacs.d/usr/6.945-config/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         MIT-scheme config                        ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the place where you have installed scheme. Be sure to set
;; this to an appropriate value!!!
(setq scheme-root "/usr/local")

(setq scheme-program-name
      (concat
       scheme-root "/bin/mit-scheme "
       "--library " scheme-root "/lib/mit-scheme-x86-64 "
       "--band " scheme-root "/lib/mit-scheme-x86-64/all.com "
       "-heap 10000"))

;; Mac OS X: Uncomment the following versions of scheme-root and
;; scheme-program-name if you installed the pre-compiled Mac binary
;;
;; (setq scheme-root "/Applications/MIT-Scheme.app/Contents/Resources")
;;
;; (setq scheme-program-name
;;       (concat
;;        scheme-root "/mit-scheme "
;;        "--library " scheme-root " "
;;        "--band " scheme-root "/all.com "
;;        "-heap 10000"))

;; generic scheme completeion
(require 'scheme-complete)
(autoload 'scheme-smart-complete "scheme-complete" nil t)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(setq lisp-indent-function 'scheme-smart-indent-function)

;; mit-scheme documentation
(require 'mit-scheme-doc)

;; Special keys in scheme mode. Use <tab> to indent scheme code to the
;; proper level, and use M-. to view mit-scheme-documentation for any
;; symbol.
(eval-after-load
 'scheme
 '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))

(eval-after-load
 'cmuscheme
 '(define-key inferior-scheme-mode-map "\t" 'scheme-complete-or-indent))

(eval-after-load
 'scheme
 '(define-key scheme-mode-map (kbd "M-.") 'mit-scheme-doc-lookup))

(eval-after-load
 'cmuscheme
 '(define-key inferior-scheme-mode-map (kbd "M-.")
    'mit-scheme-doc-lookup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Print a Buffer to PDF  (C-c C-p)         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq pdf-target-name (concat "/tmp/" (buffer-name) ".pdf"))
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " "\"" pdf-target-name "\""))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " pdf-target-name)))

(global-set-key (kbd "C-c C-p") 'print-to-pdf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Miscellaneous Settings                   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq x-select-enable-clipboard 't)
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
;; activate auto-fill-mode for various other modes
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'scheme-mode-hook 'turn-on-auto-fill)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))
;; (setq-default ispell-program-name "aspell")


;; I couldn't resist ...
;; Try executing M-x zork
(require 'malyon)
(defun zork ()
  (interactive)
  (malyon (locate-file "zork1.z5" load-path)))
