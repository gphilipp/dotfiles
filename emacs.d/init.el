;;; init.el  Gilles's  configuration file

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please... jeez
(setq inhibit-startup-screen t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-bindings
                      starter-kit-js
                      starter-kit-lisp
                      ;; Clojure & friends
                      clojure-mode
                      auto-complete
                      cider
                      clojure-cheatsheet
                      rainbow-delimiters
                      ;; Project navigation
                      projectile
                      ack-and-a-half
                      ;; Misc.
                      markdown-mode
                      twilight-theme
                      hlinum
                      evil
                      ;; switch buffers without C-x b on each
                      buffer-move
                      org)
  "A list of packages to ensure are installed at launch.")

;; Automaticaly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Load the provided Clojure start kit configurations
(load (concat user-emacs-directory "clojure-starter-kit.el"))


;; display line-numbers and highlight current line
(require 'hlinum)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white")))))

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modif
      ier 'none)

;; so that I can run lein from emacs
(setenv "PATH" (concat (getenv "PATH") ";c:\\HOMEWARE\\leiningen-2.1.3"))
(setq exec-path (append exec-path '(";c:\\HOMEWARE\\leiningen-2.1.3")))

;;; evil configuration
;;(evil-mode 1)
;;(evil-ex-define-cmd "n[ew]" 'evil-window-new)
;;(setq evil-normal-state-cursor 'hollow)
;;Exit insert mode by pressing j and then j quickly
;;(setq key-chord-two-keys-delay 0.5)
;;(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
;;(key-chord-mode 1)


(require 'erc)

;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "#clojure" "#pedestal")
     ))
(setq erc-server "irc.freenode.net"
      erc-port 6667
      erc-nick "gphilippart")
(custom-set-variables)
