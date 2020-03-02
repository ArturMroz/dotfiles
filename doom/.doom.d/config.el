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


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-opera)
;; (setq doom-theme 'doom-palenight)



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

(setq
 display-line-numbers-type 'visual
 dired-dwim-target t
 evil-snipe-scope 'buffer

 ;; neo-window-fixed-size nil

 )


(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.15))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.09))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.03))))
 ;; '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 ;; '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-document-title ((t (:height 1.25))))
 )

;; TODO numbers increase/dec

(setq
 org-ellipsis " ▾ "
 org-directory "~/Dropbox/Notes"
 org-log-done 'time
 ;; org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)"))
 org-todo-keywords '((sequence "TODO" "DONE"))

 org-capture-templates
 '(("x" "Note" entry
    (file+olp+datetree "journal.org")
    "**** [ ] %U %?" :prepend t :kill-buffer t)
   ("t" "Task" entry
    (file+headline "tasks.org" "Inbox")
    "* [ ] %?\n%i" :prepend t :kill-buffer t))

 ;; org-agenda-files
 ;; org-bullets-bullet-list '("·")
 )

(set-register ?t (cons 'file "~/Dropbox/Notes/tasks.org"))
(set-register ?c (cons 'file "~/dotfiles/doom/.doom.d/config.el"))

(map! :leader
      "x" #'org-capture
      "r r" #'jump-to-register
      "r j" #'counsel-register
      :desc "Create Devops links" "r u" (kbd ":s/stuff/other"))

(map! :n
      "C-j" #'evil-window-down
      "C-k" #'evil-window-up
      "C-l" #'evil-window-right
      "C-h" #'evil-window-left)


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
