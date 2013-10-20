(add-to-list 'load-path "~/.emacs.d/cc-mode/")
(add-to-list 'load-path "~/.emacs.d/yaml/")
(add-to-list 'load-path "~/.emacs.d/matlab-emacs/")


(autoload 'matlab-mode "matlab.el" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
 (setq matlab-indent-function t)
 (setq matlab-shell-command "matlab")

(global-set-key (kbd "M--") "// ################################################################")


(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.pde$" . cc-mode))

;; ----- Rosemacs stuff -----

;; Tell emacs where to find the rosemacs sources
;; replace the path with location of rosemacs on your system
(push "~/rosemacs" load-path) 

;; Open .rviz files in yaml-mode
(add-to-list 'auto-mode-alist '("\\.rviz$" . yaml-mode))

;; Load the library and start it up
(require 'rosemacs)
(invoke-rosemacs)

;; Optional but highly recommended: add a prefix for quick access
;; to the rosemacs commands
(global-set-key "\C-x\C-r" ros-keymap)

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

;; Adds yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; doxymacs
;; doxymacs-mode whenever we are in c-mode
(add-hook 'c-mode-common-hook'doxymacs-mode)

;; doxygen syntax highlighting
(defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
  (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; highlight TODO, FIXME, etc.
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\|todo\\):" 1 font-lock-variable-name-face t)))))

;; MATLAB mode 
;; (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;; (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
;; (setq matlab-indent-function-body t)  ; if you want function bodies indented
;; (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
;; (defun my-matlab-mode-hook ()
;;   (setq fill-column 76))		; where auto-fill should wrap
;; (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
;; (defun my-matlab-shell-mode-hook ()
;;   '())
;; (add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)

(font-lock-mode 1)

;; Turn the h key into the backward version of the d key
;; (global-set-key (kbd "C-?") 'help-command)

;; (global-set-key (kbd "M-?") 'mark-paragraph)

;; (global-set-key (kbd "C-h") 'delete-backward-char)

;; (global-set-key (kbd "M-h") 'backward-kill-word)

;; Force the global M-h binding over local nxml-mode binding
(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map (kbd "M-h") nil)))

(eval-after-load "matlab-mode"
  '(progn
     (define-key matlab-mode-map (kbd "C-h") nil)))
;; SLIME
(setq inferior-lisp-program "clisp")
(add-to-list 'load-path "~/.slime")
(require 'slime)
(slime-setup)

(global-font-lock-mode t)
(show-paren-mode 1)
(add-hook 'lisp-mode-hook '(lambda ()
			     (local-set-key (kbd "RET") 'newline-and-indent)))
