(add-to-list 'load-path "~/.emacs.d/yaml/")
(add-to-list 'load-path "~/.emacs.d/matlab-emacs/")

;; ----- Matlab -----
(autoload 'matlab-mode "matlab.el" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
 (setq matlab-indent-function t)
 (setq matlab-shell-command "matlab")

;; ----- Rosemacs stuff -----

;; Tell emacs where to find the rosemacs sources
;; replace the path with location of rosemacs on your system
(push "~/rosemacs" load-path) 

;; Load the library and start it up
(require 'rosemacs)
(invoke-rosemacs)

;; Optional but highly recommended: add a prefix for quick access
;; to the rosemacs commands
(global-set-key "\C-x\C-r" ros-keymap)

;; Adds yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
;; Open .rviz files in yaml-mode
(add-to-list 'auto-mode-alist '("\\.rviz$" . yaml-mode))

;; ----- doxymacs -----

;; doxymacs-mode whenever we are in c-mode
(add-hook 'c-mode-common-hook'doxymacs-mode)

;; doxygen syntax highlighting
(defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
  (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)


;; ----- SLIME -----

(setq inferior-lisp-program "clisp")
(add-to-list 'load-path "~/.slime")
(require 'slime)
(slime-setup)

(global-font-lock-mode t)
(show-paren-mode 1)
(add-hook 'lisp-mode-hook '(lambda ()
			     (local-set-key (kbd "RET") 'newline-and-indent)))

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

;; highlight TODO, FIXME, etc.
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\|todo\\):" 1 font-lock-variable-name-face t)))))

(global-set-key (kbd "M--") "// ################################################################")
