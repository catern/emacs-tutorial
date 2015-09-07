;; Commenting in elisp is done with a single ";"
;; Remember, in Emacs, you can move your cursor to any Elisp function
;; or variable in this file, then press C-h f or C-h v to see the
;; documentation for that function or variable (respsectively).

;;;; The next section installs my desired Emacs packages if not already present
;; (This is mainly a useful demonstration of some Emacs Lisp)
;; Start the package system by running the "package-initialize" function with no arguments
(package-initialize)

;; Set the my-package-list variable to a list of packages that I want
(setq my-package-list
      ;; A list looks like a function call, but starts with '( instead of (.
      ;; The ' is called "quote"; everything inside a "quoted" list is also "quoted".
      '(
	;; Since these package names are "quoted" by being inside a list, they are "symbols",
	;; written outside a list like this: 'better-defaults.
	;; (You can think of them like strings)
	better-defaults
	cyberpunk-theme 
	undo-tree
	c0-mode
	auctex
	magit
	ix
	))


;; Set the package-archives variable to a list of pairs describing the
;; package archives that I use; the package system uses this variable,
;; so changing it is like changing a setting or preference.
(setq package-archives
      ;; One can nest lists and pairs without quoting again each time,
      ;; since everything inside a quoted list is already quoted.
      '(
	;; GNU ELPA, the default package archive, with GNU packages
	("gnu" . "http://elpa.gnu.org/packages/")
	;; Marmalade, a real repository with many released packages
	("marmalade" . "http://marmalade-repo.org/packages/")
	;; MELPA, unstable packages scraped straight off of Github
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Download repository metadata, if the package-archive-contents
;; variable isn't set.  package-archive-contents is loaded from a
;; cache on disk if possible by package-initialize.
;; Unless we already have package metadata...
(unless package-archive-contents 
  ;; ...run package-refresh-contents with no arguments to get metadata
  (package-refresh-contents))

;; Install the missing packages
;; Set the "package" variable to each element in my-package-list:
;; (dolist is like "map" in SML, or "for" in Python)
(dolist (package my-package-list)
  ;; Then, unless the package is already installed...
  (unless (package-installed-p package)
    ;; ...install the package.
    (package-install package)))

;; Tell the startup system not to automatically run package-initialize
;; after init.el, since we already ran it. This is just a small
;; startup time optimization.
(setq package-enable-at-startup nil)

;;;; End of section installing packages

;;;; Visuals
;; Enable cyberpunk theme! HACK THE PLANET
;; 'cyberpunk is a symbol (kind of like a string)
(load-theme 'cyberpunk t)

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Scroll incrementally, not screenfulls at a time
(setq scroll-conservatively 1000)

;;;; Undo better-defaults changes
;; Re-enable the menu-bar, tool-bar, and scroll-bar, disabled by better-defaults
;; If you grow to not like them, just remove these lines
(menu-bar-mode 1)
(tool-bar-mode 1)
(scroll-bar-mode 1)

;;;; undo-tree is great, let's enable it
;; Remember, you can do C-h m to see descriptions for all active
;; modes; scroll to the undo-tree information to see what it adds
;; undo-tree-visualize is great!
(global-undo-tree-mode)
