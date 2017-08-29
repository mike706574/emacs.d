;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t )
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(
    ;; general
    auto-complete
    company
    idle-highlight-mode
    ido-ubiquitous
    magit
    markdown-mode
    paredit
    powerline
    powershell
    projectile
    smex
    tagedit
    web-mode
    yaml-mode

    ;; clojure
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    ac-cider
    clj-refactor
    clojure-cheatsheet

    ;; docker
    dockerfile-mode

    ;; elixir
    elixir-mode
    alchemist

    ;; elm
    elm-mode

    ;; erlang
    erlang

    ;; f#
    fsharp-mode

    ;; gradle
    gradle-mode

    ;; groovy
    groovy-mode

    ;; haskell
    haskell-mode
    hindent

    ;; javascript
    js2-mode
    rjsx-mode
    ))


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Load up everything in the vendor directory
(dolist (project (directory-files "~/.emacs.d/vendor" t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 'auto-complete-config)
(ac-config-default)

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")
(load "log.el")

;; Build tools
(load "maven.el")
(load "lein.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Language-specific
(load "setup-clojure.el")
(load "setup-elixir.el")
(load "setup-groovy.el")
(load "setup-haskell.el")
(load "setup-js.el")
(load "setup-misc-programming.el")
(load "setup-web.el")

;; I like doing dangerous things
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-firefox))
 '(cljr-favor-prefix-notation nil)
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" default)))
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(magit-git-executable "/usr/bin/git")
 '(package-selected-packages
   (quote
    (powershell yaml-mode web-mode tagedit smex rjsx-mode projectile powerline markdown-mode magit ido-ubiquitous idle-highlight-mode hindent haskell-mode groovy-mode fsharp-mode evil-numbers erlang elm-mode clojure-mode-extra-font-locking clojure-cheatsheet clj-refactor alchemist ac-cider))))

;; Custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-3 ((t (:background "peach puff")))))

(menu-bar-mode -1)
