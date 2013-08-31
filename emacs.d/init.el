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
                      nrepl
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
 '(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white"))) t))

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; so that I can run lein from emacs
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;; evil configuration
;;(evil-mode 1)
;;(evil-ex-define-cmd "n[ew]" 'evil-window-new)
;;(define-key evil-insert-state-map (kbd "j f") 'evil-normal-state)
;;(setq evil-normal-state-cursor 'hollow)

;; IRC client
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

;;Exit insert mode by pressing j and then j quickly
;;(setq key-chord-two-keys-delay 0.5)
;;(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
;;(key-chord-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e3e2db3b5acd2028f7f83581f9609e1e7369df20414ab53c9b2161e2eca08675" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "a234f91f9be6ed40f6ce0e94dce5cea1b9f1ccec2b9ccd42bb71c499867a3fcc" "a5a1e3cd5f790846f4eec5fcff52935e5ef6d713a0f9342fef12eccfd9e9eff0" "5195dfc4aa4e8ff66248b9ba08983da04aff1d82e680fb9e008091fdb39d7c76" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "c377a5f3548df908d58364ec7a0ee401ee7235e5e475c86952dc8ed7c4345d8e" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4" "ad9b960b3cf80ced640ea8277ec1143be76f7f70d9a062d9a882333d39d2d2f6" "d6d8a574d826c260b23c487443cc0a904d00db791cf948777a559f1c2c05fecd" "78cfbd96775588c06c4fff22573aaa5fa762ca2b8eda43cb964b7739194ed3c1" "06f5145c01ec774a0abb49eeffa3980743ce2f997112b537effeb188b7c51caf" "8c5ffc9848db0f9ad4e296fa3cba7f6ea3b0e4e00e8981a59592c99d21f99471" "60a2ebd7effefeb960f61bc4772afd8b1ae4ea48fae4d732864ab9647c92093a" "1a093e45e4c3e86fa5ad1f8003660e7cda4d961cd5d377cee3fee2dad2faf19b" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "8951592dc830d55c01d1d934f90b852037bd210ef16b06e12b4f91f582fd4227" "5339210234ec915d7d3fd87bfeb506bfc436ff7277a55516ab1781ec85c57224" "9ea054db5cdbd5baa4cda9d373a547435ba88d4e345f4b06f732edbc4f017dc3" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "fe0a47cc3952fede574527a1c28ddf3a1af381fc1fb5843ca60d22e4c841011a" "ea0c5df0f067d2e3c0f048c1f8795af7b873f5014837feb0a7c8317f34417b04" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "88d556f828e4ec17ac074077ef9dcaa36a59dccbaa6f2de553d6528b4df79cbd" "2e60db7f24913de7cea9d719dc25fcf6b45682bef4693e35aec88aed3da1443e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "f61972772958e166cda8aaf0eba700aad4faa0b4101cee319e894e7a747645c9" "50ceca952b37826e860867d939f879921fac3f2032d8767d646dd4139564c68a" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "75d4ccc5e912b93f722e57cca3ca1a15e079032cd69fd9bc67268b4c85639663" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
