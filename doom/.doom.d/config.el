;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 17)
      doom-variable-pitch-font (font-spec :family "sans" :size 14))
;; (setq doom-font (font-spec :family "Hack Regular Nerd Font Complete Mono" :size 16)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.

(setq doom-theme 'doom-gruvbox)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-register ?t (cons 'file "~/Dropbox/Notes/tasks.org"))
(set-register ?c (cons 'file "~/dotfiles/doom/.doom.d/config.el"))

(map! :leader
      ";"   #'counsel-M-x
      ":"   #'pp-eval-expression
      "x"   #'org-capture
      "a"   #'am/open-agenda
      "/"   #'rg-menu

      "b n" #'evil-buffer-new
      "f j" #'counsel-file-jump
      "i d" #'evil-insert-digraph
      "o c" #'quick-calc
      "s g" #'rg-dwim
      "s a" #'swiper-all
      "r r" #'jump-to-register
      "r n" #'evil-ex-nohighlight
      "r c" #'org-update-all-dblocks
      )

(map! :i "C-v" #'evil-paste-before
      :i "C-k" #'evil-insert-digraph

      :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right
      )

(map! :map neotree-mode-map
      :n "}" #'neotree-select-next-sibling-node
      :n "{" #'neotree-select-previous-sibling-node
      )

(setq
 company-box-doc-enable nil
 dired-dwim-target t
 display-line-numbers-type 'visual
 doom-modeline-buffer-file-name-style 'truncate-with-project
 doom-scratch-initial-major-mode 'org
 doom-themes-neotree-file-icons t
 evil-snipe-scope 'buffer
 magit-ediff-dwim-show-on-hunks t
 neo-window-fixed-size nil
 rg-command-line-flags '("--max-columns=150")
 )

(rg-enable-default-bindings)
(rg-define-toggle "-uu" "I" nil)

(after! org
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.09))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.06))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.03))))
   '(org-document-title ((t (:height 1.25))))
   )

  (setq
   org-directory "~/Dropbox/Notes"
   org-agenda-files '("~/Dropbox/Notes/tasks.org"
                      ;; "~/Dropbox/Notes/tasks.org"
                      "~/Dropbox/Notes/exercism.org")
   org-log-done 'time
   org-ellipsis " » "
   ;; org-bullets-bullet-list '("◉" "◎" "○" "✿" "•" )
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("1." . "-"))
   org-plain-list-ordered-item-terminator ?.
   org-tags-column -80

   org-agenda-skip-scheduled-if-done t
   org-agenda-use-time-grid nil
   org-blank-before-new-entry '((heading) (plain-list-item))
   org-log-done 'time
   org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "WAIT(w)" "|" "DONE(d)" "KILL(k)"))

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
   '(("j" "Journal" entry
      (file+olp+datetree "journal.org")
      "**** [ ] %U %?" :prepend t :kill-buffer t)
     ("n" "Note" entry
      (file+headline "notebook.org" "Inbox")
      "** %U %?" :prepend t :kill-buffer t)
     ;; tasks
     ("e" "Emacs task" checkitem
      (file+headline "tasks.org" "Emacs stuff")
      "- [ ] %?" :prepend t)
     ("x" "Task" entry
      (file "tasks.org")
      "** TODO %?\n%i" :prepend t :kill-buffer t)
     ("c" "Task with clock" entry
      (file+headline "tasks.org" "Backlog")
      "** STRT %?" :prepend t :clock-in t :clock-keep t)
     )))

(defun am/open-agenda ()
  (interactive)
  (org-agenda nil "c"))
