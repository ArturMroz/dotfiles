;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; SETS ----------------------------------


(setq
 user-full-name "John Doe"
 user-mail-address "john@doe.com"

 ;; doom-font (font-spec :family "Fira Code" :size 17)
 doom-font (font-spec :family "Iosevka" :size 18)
 doom-variable-pitch-font (font-spec :family "sans" :size 14)
 doom-modeline-buffer-file-name-style 'truncate-with-project
 ;; doom-scratch-initial-major-mode 'org
 doom-themes-neotree-file-icons t
 doom-theme 'doom-gruvbox

 ;; company-box-doc-enable nil
 dired-dwim-target t
 display-line-numbers-type 'visual
 evil-snipe-scope 'buffer
 flycheck-jshintrc "~/.emacs.d/.jshintrc"
 httpd-root "~/Documents/code/little-bits/snek"
 magit-ediff-dwim-show-on-hunks t
 ;; neo-window-fixed-size nil
 ;; org-pomodoro-format "%s"
 ;; rainbow-delimiters-max-face-count 6
 rg-command-line-flags '("--max-columns=150")
 scroll-preserve-screen-position nil ; Avoid jump when search
 show-trailing-whitespace t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-register ?t (cons 'file "~/Dropbox/Notes/tasks.org"))
(set-register ?c (cons 'file "~/dotfiles/doom/.doom.d/config.el"))

(set-popup-rule! "^\\*rg\\*" :side 'bottom :size 1.00 :select t :ttl nil)


;; MAPS -----------------------------------

(map! :leader
      "/" #'rg-menu
      ":" #'pp-eval-expression
      ";" #'counsel-M-x
      "a" #'am/open-agenda
      "k" #'am/select-clock
      "x" #'org-capture

      "b n" #'evil-buffer-new
      "f j" #'counsel-file-jump
      "g b" #'dumb-jump-go
      "i d" #'evil-insert-digraph
      "o =" #'ranger
      "o c" #'quick-calc
      "s g" #'rg-dwim
      "s a" #'swiper-all
      "w <up>" #'am/fibonacci-resize

      (:prefix ("r" . "my stuff")
       "c" #'org-update-all-dblocks
       "d" #'org-clock-display
       "j" #'am/toggle-narrow-js2
       "J" #'am/widen-to-web-mode
       "n" #'evil-ex-nohighlight
       "r" #'jump-to-register
       "k" #'am/update-cookies))

(map! :i "C-v" #'evil-paste-before
      :i "C-k" #'evil-insert-digraph

      :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right)

(map! :map neotree-mode-map
      :n "}" #'neotree-select-next-sibling-node
      :n "{" #'neotree-select-previous-sibling-node)


;; FUNCTIONS ------------------------------

(defun am/update-cookies ()
  (interactive)
  (org-update-statistics-cookies t))

(defun am/print-file-name ()
  (interactive)
  (message (buffer-file-name)))

(defun am/open-agenda ()
  (interactive)
  (org-agenda nil "c"))

(defun am/select-clock ()
  (interactive)
  (org-clock-select-task))

(defun am/fibonacci-resize ()
  (interactive)
  (enlarge-window-horizontally (round (* 0.33 (window-total-width)))))

(defun am/set-httpd-root-to-curr-dir ()
  (interactive)
  (let ((curr-dir (file-name-directory buffer-file-name)))
    (setq httpd-root curr-dir)
    (message (format "Httpd root set to: %s" curr-dir))))

(defun am/toggle-narrow-js2 ()
  (interactive)
  (if (buffer-narrowed-p)
      (progn
        (widen)
        (web-mode))
    (execute-kbd-macro (kbd "znit"))
    (js2-mode)))

(defun am/widen-to-web-mode ()
  (interactive)
  (doom/widen-indirectly-narrowed-buffer)
  (web-mode))


;; HOOKS

;; (add-hook 'html-mode-hook 'skewer-html-mode)

;; (with-eval-after-load 'outline
;;   (add-hook 'ediff-prepare-buffer-hook #'org-show-all))

;; (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(advice-add #'load-theme :after
            (lambda (&rest _)
              (set-face-italic 'font-lock-comment-face t)))

(add-hook 'js2-mode-hook
          (defun my-js2-mode-setup ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint))))


;; PACKAGES -------------------------------

(rg-enable-default-bindings)
(rg-define-toggle "-uu" "I" nil)

(after! web-mode
  (setq web-mode-markup-indent-offset 2))

(after! org
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.09))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.06))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.03))))
   '(org-document-title ((t (:height 1.25)))))

  (setq
   org-directory "~/Dropbox/Notes"
   org-agenda-files '("~/Dropbox/Notes/tasks.org"
                      "~/Dropbox/Notes/exercism.org")
   org-ellipsis " » "
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("1." . "-"))
   org-plain-list-ordered-item-terminator ?.
   org-tags-column -80

   org-agenda-skip-scheduled-if-done t
   org-agenda-use-time-grid nil
   ;; org-blank-before-new-entry '((heading) (plain-list-item))
   org-log-done 'time
   org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "WAIT(w)" "|" "DONE(d)" "KILL(k)"))
   org-reverse-note-order t

   org-agenda-custom-commands
   '(("c" "Agenda and TODOs"
      ((agenda "")
       (alltodo
        ""
        ((org-agenda-overriding-header "\nTasks")
         (org-agenda-sorting-strategy
          '(todo-state-down
            priority-down
            time-down
            effort-up
            category-keep)))))))

   org-capture-templates
   '(("n" "Note" entry
      (file+headline "notebook.org" "Inbox")
      "** %U %?" :prepend t :kill-buffer t)
     ("x" "Task" entry
      (file "tasks.org")
      "** TODO %?\n%i" :prepend t :kill-buffer t)
     ("d" "Task for today" entry
      (file "tasks.org")
      "** TODO %?\nSCHEDULED: %u%i" :prepend t :kill-buffer t)
     ("c" "Task with clock" entry
      (file+headline "tasks.org" "Backlog")
      "** STRT %?" :prepend t :clock-in t :clock-keep t)
     ("e" "Emacs task" checkitem
      (file+headline "tasks.org" "Emacs stuff")
      "- [ ] %?" :prepend t))))


;; MISC -----------------------------------

;; Additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (font-spec :family "Hack NF" :size 15))
;; (font-spec :family "Roboto Mono" :size 15))
