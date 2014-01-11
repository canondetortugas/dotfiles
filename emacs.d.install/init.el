(add-to-list 'load-path "~/.emacs.d/misc")
(add-to-list 'load-path "~/.emacs.d/yaml/")
(add-to-list 'load-path "~/.emacs.d/matlab-emacs/")
;; ----- Matlab -----
(autoload 'matlab-mode "matlab.el" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
 (setq matlab-indent-function t)
 (setq matlab-shell-command "matlab")

;; ----- rosemacs stuff -----

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
;; Each line adds a key-value pair (e.g. ("\\.yml$" . yaml-mode) to
;; an associative array (auto-mode-alist)
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
(add-hook 'slime-repl-mode-hook '(lambda ()
				   (local-set-key (kbd "C-c x") 'slime-repl-quit)))

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
    (otherwise (list comment-start comment-end))
    )
  )

(defun my-comment-start ()
  (first (my-comment) ) )

(defun my-comment-end()
  (car (last (my-comment) ) ) )

(defvar comment-line-length 100 "Line length that we will fill out with comment characters")

(defun comment-line ()
  (interactive)
  (if (use-region-p)
      nil
    (let ((eol-pos (+ (line-beginning-position) comment-line-length))
	  (comment-start (my-comment-start))
	  (comment-end (my-comment-end))
	  )
      (end-of-line)
      (when (/= (point) (line-beginning-position) )
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


(global-set-key (kbd "M--") 'comment-line)
(global-set-key (kbd "C-c c") 'fancy-comment)
(global-set-key (kbd "C-c t") 'insert-todo)


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
(defvar latex-mode-extensions-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c d") (lambda ()
				   (interactive) (insert "$$") (backward-char 1)))
    km)
  )

(defvar latex-mode-extensions nil)

(push (cons 'latex-mode-extensions latex-mode-extensions-keymap) minor-mode-map-alist)

(add-hook 'latex-mode-hook
	  (lambda ()
	    (setq latex-mode-extensions t) ) )

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
