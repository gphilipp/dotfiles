;; General
(setq initial-scratch-message nil)                                         ; *scratch* starts empty
(when (locate-library "clojure-mode")                                      ; Set *scratch* to Clojure mode
  (setq initial-major-mode 'clojure-mode))

(projectile-global-mode)                                                   ; Quickly navigate projects using Projectile (C-c p C-h for available commands)
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths) ; Projectile shows full relative paths

;; Visual
(load-theme 'birds-of-paradise-plus  t)                                    ; Load my preferred theme
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)                        ; Enable rainbow delimiters when programming
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)                    ; Disable emacs-starter-kits line highlighting
(set-face-attribute 'default nil :family "Source Code Pro Medium" :height 140)           ; Use 10-pt Consolas as default font

(global-linum-mode t)                                                      ; Always show line numbers on left
(setq linum-format "%4d ")                                                 ; Line numbers gutter should be four characters wide

(line-number-mode 1)                                                       ; Mode line shows line numbers
(column-number-mode 1)                                                     ; Mode line shows column numbers

(setq-default tab-width 2)                                                 ; Tab width of 2
(fset 'yes-or-no-p 'y-or-n-p)                                              ; Emacs prompts should accept "y" or "n" instead of the full word

(setq visible-bell nil)                                                    ; No more Mr. Visual Bell Guy.

;; Clojure
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))  ; *.edn are Clojure files
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist)) ; *.cljs are Clojure files


;; cider customizations
(setq cider-repl-tab-command 'cider-repl-indent-and-complete-symbol)
(setq nrepl-hide-special-buffers t)                                        ; Don't show buffers like connection or server
(setq cider-popup-on-error nil)                                            ; Don't popup new buffer for errors (show in nrepl buffer)
(setq cider-popup-stacktraces t)                                           ; Display stacktrace inline
(setq cider-repl-print-length 100)                                         ; Limit the output 
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)          ; Enable eldoc - shows fn argument list in echo area
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)                      ; Enable eldoc in Clojure buffers
(add-hook 'cider-repl-mode-hook 'paredit-mode)                             ; paredit in *nrepl* buffer
;; (add-to-list 'same-window-buffer-names "*nrepl*")                       ; Make C-c C-z switch to *nrepl*


;; Ido-mode customizations
(setq ido-decorations                                                      ; Make ido-mode display vertically
      (quote
       ("\n-> "           ; Opening bracket around prospect list
        ""                ; Closing bracket around prospect list
        "\n   "           ; separator between prospects
        "\n   ..."        ; appears at end of truncated list of prospects
        "["               ; opening bracket around common match string
        "]"               ; closing bracket around common match string
        " [No match]"     ; displayed when there is no match
        " [Matched]"      ; displayed if there is a single match
        " [Not readable]" ; current diretory is not readable
        " [Too big]"      ; directory too big
        " [Confirm]")))   ; confirm creation of new file or buffer

(add-hook 'ido-setup-hook                                                  ; Navigate ido-mode vertically
          (lambda ()
            (define-key ido-completion-map [down] 'ido-next-match)
            (define-key ido-completion-map [up] 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

(require 'paredit)

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)

