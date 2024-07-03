;; ____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(message "Hello-world -- this is my init.el file loading!")

(add-to-list 'load-path "~/.emacs.d/misc")
(add-to-list 'load-path "~/.emacs.d/yaml/")
(add-to-list 'load-path "~/.emacs.d/matlab-emacs/")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list 'load-path "~/.emacs.d/elpa")

;; C++
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; Open in full screen by default.
(set-frame-parameter nil 'fullscreen 'fullboth)

;; OCAML
;; (load "/Users/nalyd/.opam/system/share/emacs/site-lisp/tuareg-site-file")

;; Color Theme (in X mode)
;; (if (or (eq 'x window-system) (eq 'ns window-system))
(progn (require 'color-theme)
       (color-theme-initialize)
       (color-theme-classic))

;; ;; color Theme (in X mode)
;; (if (or (eq 'x window-system) (eq 'ns window-system))
;;     (progn (require 'color-theme)
;; 	   (color-theme-initialize)
;; 	   (color-theme-charcoal-black)))

;; Color Theme (in X mode)
(if (or (eq 'x window-system) (eq 'ns window-system))
    (load-theme 'nimbus t)
    ;; (use-package nimbus-theme)
  )
;; Sets current color theme as default for new frames (not tested).
(if (boundp 'aquamacs-version)
      (aquamacs-set-frame-parameters-as-default))


;; ----- Matlab -----
(autoload 'matlab-mode "matlab.el" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
 (setq matlab-indent-function t)
;; Make matlab-mode find the matlab executable
 (setq matlab-shell-command "/Applications/MATLAB_R2013a.app/bin/matlab")

;; Adds yaml mode
(require 'yaml-mode)
;; Each line adds a key-value pair (e.g. ("\\.yml$" . yaml-mode) to
;; an associative array (auto-mode-alist)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)) 
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
;; Open .rviz files in yaml-mode
(add-to-list 'auto-mode-alist '("\\.rviz$" . yaml-mode))

;; ----- doxymacs -----

;; doxymacs-mode whenever we are in c-mode
;; (add-hook 'c-mode-common-hook'doxymacs-mode)

;; doxygen syntax highlighting
;; (defun my-doxymacs-font-lock-hook ()
;;     (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;         (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

(show-paren-mode 1)

;; ----- Yank to clipboard -----

(setq x-select-enable-clipboard t)
(defun yank-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
        (progn
	  (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
	  (message "Yanked region to clipboard!")
	  (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(global-set-key [f8] 'yank-to-x-clipboard)

;; ----- Misc. -----

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO: Add TODO highlighting for other modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Author: Dylan Foster, Date: 2014-01-09 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; highlight TODO, FIXME, etc.
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\|todo\\):" 1 font-lock-variable-name-face t)))))

(defun my-comment ()
  (case major-mode
    ('c++-mode (list "//" "/"))
    ('c-mode (list "//" "/"))
    ('yaml-mode (list "#" "#"))
    (otherwise (list comment-start comment-end))
    )
  )

(defun my-comment-start ()
  (first (my-comment) ) )

(defun my-comment-end()
  (car (last (my-comment) ) ) )

(defvar comment-line-length 60 "Line length that we will fill out with comment characters")

(defun comment-line ()
  (interactive)
  (if (use-region-p)
      nil
    (let ((eol-pos (+ (line-beginning-position) comment-line-length))
	  (comment-start (my-comment-start))
	  (comment-end (my-comment-end))
	  )
      (end-of-line)
      ;; Don't insert whitespace if the line contains only whitespace
      (unless (string-match "^[ \t]*" (buffer-substring-no-properties (line-beginning-position) (lne-end-position)) )
	(insert " "))
      (do ()
	  ((< eol-pos (point)))
	(insert comment-start) )
      (when (= eol-pos (point))
	(if (string= comment-end "")
	    (insert comment-start)
	  (insert comment-end)
	  )
	)
      (newline-and-indent)
      ) 
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO: Add more lines if the comment won't fit on one line ;;;;;;;;;;;;;;;;;;;;;
; Author: Dylan Foster, Date: 2014-01-09 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fancy-comment (comment-string)
  (interactive "MComment: ")
  (newline-and-indent)
  (comment-line)
  (insert (concatenate 'string (my-comment-start) " " comment-string ))
  (comment-line)
  (comment-line)
)

(defvar todo-author "Dylan Foster")
(defun insert-todo (comment-string)
  (interactive "MComment: ")
  (newline-and-indent)
  (comment-line)
  (insert (concatenate 'string (my-comment-start) 
		       " TODO: " comment-string ) )
  (comment-line)
  (insert (concatenate 'string (my-comment-start) 
		       " Author: " todo-author ", Date: " (format-time-string "%Y-%m-%d") ) )
  (comment-line)

  (comment-line)
  )

;; turned off for now
;; (global-set-key (kbd "M--") 'comment-line)
;; (global-set-key (kbd "C-c c") 'fancy-comment)
;; (global-set-key (kbd "C-c t") 'insert-todo)


;; Cycle through windows in reverse
(defun backward-window () 
  "Move to the previous window"
  (interactive)
  (other-window -1) )
(global-set-key (kbd "C-x p") 'backward-window)

;; comment-end
;; (set-cursor-color "red")
;; (send-string-to-terminal "\033]12;red\007")
;; (add-hook 'window-setup-hook '(lambda ()
;; 				(set-cursor-color "red")))

;; Shortcuts for examining unbound keys
(require 'unbound)

;; ----- emerge -----

;; Only works with diff, not diff3
;; (setq emerge-diff-options "--ignore-all-space")

;; ----- latex -----

;; (add-hook 'latex-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key (kbd "C-c d")
;; 			   (lambda ()
;; 			     (interactive) (insert "$$") (backward-char 1)))))



;; TODO: This doesn't currently get disabled on leaving latex-mode
;; (defvar latex-mode-extensions-keymap
;;   (let ((km (make-sparse-keymap)))
;;     (define-key km (kbd "C-c d") (lambda ()
;; 				   (interactive) (insert "$$") (backward-char 1)))
;;     km)
;;   )

;; (defvar latex-mode-extensions nil)

;; (push (cons 'latex-mode-extensions latex-mode-extensions-keymap) minor-mode-map-alist)

;; (add-hook 'latex-mode-hook
;; 	  (lambda ()
;; 	    (setq latex-mode-extensions t) ) )

;; AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -r -g %n %o %b")))
;; (setq TeX-view-program-list
;;       '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; (require 'tex)
;; (TeX-global-PDF-mode t)

;; FlyMake LaTeX
;; (require 'flymake)

;; (defun flymake-get-tex-args (file-name)
;; (list "pdflatex"
;; (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;; (add-hook 'LaTeX-mode-hook 'flymake-mode)


;; LaTeX info - C-h S looks up documentation for TeX symbol at point
(require 'info-look)
(info-lookup-add-help
 :mode 'latex-mode
 :regexp ".*"
 :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
 :doc-spec '(("(latex2e)Concept Index" )
	     ("(latex2e)Command Index")))

;; Add LaTeX preview pane package to archive
(require 'package)
(add-to-list 'package-archives
	     ;; '("melpa-stable" . "https://stable.melpa.org/packages/") t)
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
	     ;; '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ;org-mode archives

;; XML Mode
;; Automatically close 
(add-hook 'nxml-mode-hook
	  (lambda ()
	    (setq nxml-slash-auto-complete-flag t)))

;; Open this file
(global-set-key (kbd "C-c e") 
		(lambda ()
		  (interactive)
		  (find-file "~/.emacs.d/init.el" ) ) )

(defun insert-depends (package)
  (interactive "MPackage: ")
  (insert "<build_depend>" package "</build_depend>\n")
  (indent-for-tab-command)
  (insert "<run_depend>" package "</run_depend>\n" ) 
  (indent-for-tab-command))

(add-hook 'nxml-mode-hook
	  (lambda () 
	  (local-set-key (kbd "C-c d") 'insert-depends ) ))
;; iPython
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n") ;

;; org-mode
(add-hook 'org-mode-hook 'turn-on-org-cdlatex) ;latex shortcuts
;; org capture
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
;; (require 'org-journal)
;; (setq org-journal-dir "~/workspace/journal")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((TeX-master . "paper")
     (TeX-master . "../richlqr_neurips2020.tex")
     (TeX-master . t)
     (TeX-master . thesis))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Turns on global-auto-revert-mode by default.
(global-auto-revert-mode 1)
