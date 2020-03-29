;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
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
;; test
;; (setq doom-font (font-spec :family "Hack Regular Nerd Font Complete Mono" :size 16)
;; (setq doom-font (font-spec :family "monospace" :size 16)
(setq doom-font (font-spec :family "Fira Code" :size 17)
      doom-variable-pitch-font (font-spec :family "sans"))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-opera)
(setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-palenight)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-register ?t (cons 'file "~/Dropbox/Notes/tasks.org"))
(set-register ?c (cons 'file "~/dotfiles/doom/.doom.d/config.el"))

(map! :leader
      ":"   #'pp-eval-expression
      ";"   #'counsel-M-x
      "x"   #'org-capture
      "a"   #'am/open-agenda
      "/"   #'rg-menu
      "b n" #'evil-buffer-new
      "f j" #'counsel-file-jump
      "i d" #'evil-insert-digraph
      "o c" #'quick-calc
      "s g" #'rg-dwim
      "s a" #'swiper-all

      (:prefix "r"
        "r" #'jump-to-register
        "j" #'counsel-register
        "n" #'evil-ex-nohighlight
        "c" #'org-update-all-dblocks

        ;; :desc "Create devops link" "l" (kbd ":s/\\d{4,5}/[[devops:&][&]]")
        :desc "Shorten 'User Story'" "u" (kbd ":s/User SPC Story/US")))

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
 doom-themes-neotree-file-icons t
 evil-snipe-scope 'buffer
 doom-scratch-initial-major-mode 'org
 magit-ediff-dwim-show-on-hunks t
 neo-window-fixed-size nil
 )

(rg-enable-default-bindings)
(rg-define-toggle "-uu" "I" nil)
(setq rg-command-line-flags
      '("--max-columns=150"
        ;; "--max-columns-preview"
        ))

;; (after! ivy-posframe
;;   (setq ivy-posframe-display-functions-alist
;;         '((counsel-git-grep . ivy-display-function-fallback)
;;           (counsel-grep . ivy-display-function-fallback)
;;           (counsel-rg . ivy-display-function-fallback)
;;           (counsel-describe-variable . ivy-display-function-fallback)
;;           (counsel-describe-function . ivy-display-function-fallback)
;;           (swiper . ivy-display-function-fallback)
;;           (swiper-isearch . ivy-display-function-fallback)
;;           ;; (t . +ivy-display-at-frame-center-near-bottom-fn))
;;           (t . ivy-posframe-display-at-frame-center))))

(after! org
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.09))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.06))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.03))))
   ;; '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   ;; '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-document-title ((t (:height 1.25))))
   )

  ;; TODO numbers increase/dec

  (setq
   org-ellipsis " » " ; " ▾ "
   org-directory "~/Dropbox/Notes"
   org-agenda-files '("~/Dropbox/Notes/tasks.org"
                      ;; "~/Dropbox/Notes/tasks.org"
                      "~/Dropbox/Notes/exercism.org")
   org-log-done 'time
   ;; org-bullets-bullet-list '("◉" "◎" "○" "✿" "•" )
   ;; org-bullets-bullet-list '("·")
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("1." . "-"))
   org-plain-list-ordered-item-terminator ?.
   org-tags-column -80

   org-agenda-skip-scheduled-if-done t
   org-agenda-use-time-grid nil
   org-blank-before-new-entry '((heading) (plain-list-item))
   org-log-done 'time
   org-log-redeadline 'time
   org-log-reschedule 'time
   org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "WAIT(w)" "|" "DONE(d)" "KILL(k)"))
   ;; org-capture-templates
   ;; '(("x" "Note" entry
   ;;    (file+olp+datetree "journal.org")
   ;;    "**** [ ] %U %?" :prepend t :kill-buffer t)
   ;;   ("t" "Task" entry
   ;;    (file+headline "tasks.org" "Inbox")
   ;;    "* [ ] %?\n%i" :prepend t :kill-buffer t))

   org-agenda-custom-commands
   '(("c" "Agenda and TODOs"
      ((agenda "")
       (alltodo
        ""
        ((org-agenda-overriding-header "\nTasks")
         ;; (org-agenda-block-separator "---")
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
     ("s" "Testing task with clock" entry
      (file+headline "tasks.org" "Backlog")
      "** STRT :t %x :test:" :prepend t :clock-in t :clock-keep t)
     ;; ("c" "Task with clock" entry
     ;;  (file+headline "tasks.org" "Backlog")
     ;;  "** STRT %?" :prepend t :clock-in t :clock-keep t)
     ;; ("d" "Task for today" entry
     ;;  (file+headline "tasks.org" "Backlog")
     ;;  "** TODO %?\n%i SCHEDULED: %t" :prepend t :kill-buffer t)
     ("x" "Task" entry
      (file "tasks.org")
      "** TODO %?\n%i" :prepend t :kill-buffer t)

     ;; ("f" "Testing task with clock" entry
     ;;  (file+headline "tasks.org" "Backlog")
     ;;  "** STRT :t %(am/replace \"%x\") :test: \n heyo %x" :prepend t)

     )
   ))

(defun am/open-agenda ()
  (interactive)
  (org-agenda nil "c"))

;; (defun am/replace (mytext)
;;   (s-truncate 70
;;               (s-replace "User Story" "US"
;;                          (s-replace-regexp "\\([0-9]\\{4,5\\}\\)"
;;                                            "[[devops:\\1][\\1]]"
;;                                            mytext))))

;; (("t" "Personal todo" entry
;;   (file+headline +org-capture-todo-file "Inbox")
;;   "* [ ] %?\n%i\n%a" :prepend t)
;;  ("n" "Personal notes" entry
;;   (file+headline +org-capture-notes-file "Inbox")
;;   "* %u %?\n%i\n%a" :prepend t)
;;  ("j" "Journal" entry
;;   (file+olp+datetree +org-capture-journal-file)
;;   "* %U %?\n%i\n%a" :prepend t)
;;  ("p" "Templates for projects")
;;  ("pt" "Project-local todo" entry
;;   (file+headline +org-capture-project-todo-file "Inbox")
;;   "* TODO %?\n%i\n%a" :prepend t)
;;  ("pn" "Project-local notes" entry
;;   (file+headline +org-capture-project-notes-file "Inbox")
;;   "* %U %?\n%i\n%a" :prepend t)
;;  ("pc" "Project-local changelog" entry
;;   (file+headline +org-capture-project-changelog-file "Unreleased")
;;   "* %U %?\n%i\n%a" :prepend t)
;;  ("o" "Centralized templates for projects")
;;  ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
;;  ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
;;  ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))

(eval-when-compile
  (require 'cl))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))
