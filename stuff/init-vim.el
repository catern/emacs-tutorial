;;;; Install my desired packages if not present
(require 'package)

;; configure desired repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;; start the package system
(package-initialize)

;; load the list of packages available (if necessary)
(unless package-archive-contents 
  (package-refresh-contents))

;; list the packages I want
(setq my-package-list '(better-defaults
			undo-tree
			evil
			cyberpunk-theme))

;; install the missing packages
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;; Visuals
;; enable cyberpunk theme yeah!
(load-theme 'cyberpunk t)

;; enable syntax highlighting
(global-font-lock-mode t)

;; scroll incrementally, not screenfulls at a time
(setq scroll-conservatively 1000)

;; don't ring the bell
(setq ring-bell-function 'ignore)

;;;; backups, autosaves and save places
(setq
 ;; put backups in .emacs.d, not in the directory of the original file
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 ;; since we are putting backups in our homedir, we might need to
 ;; cross filesystems; a cross-filesystem rename is just a copy so we
 ;; might as well copy all the time
 backup-by-copying t
 ;; keep lots of versions and don't complain about it
 kept-new-versions 6
 kept-old-versions 2
 delete-old-versions t
 ;; number our backups in order
 version-control t
 ;; also put autosaves in .emacs.d
 auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/\\1" t))
 )
;; make sure autosave directory exists
(make-directory "~/.emacs.d/autosaves/" t)

;;;; evil-mode configuration
(require 'undo-tree)
(require 'evil)

;; turn it on by default
(evil-mode 1)

;; be a little more lenient to tolerate key delay over ssh
(setq evil-esc-delay .05)
