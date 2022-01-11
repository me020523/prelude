;;; prelude-org.el --- Emacs Prelude: org-mode configuration.
;;
;; Copyright © 2011-2021 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(prelude-require-package 'org-bullets)
(prelude-require-package 'org-roam)
(prelude-require-package 'org2ctex)
(prelude-require-package 'org-contrib)

(require 'org)
(require 'org-roam)
(require 'org2ctex)
(require 'ox-taskjuggler)

;;(prelude-require-package 'org-roam-ui)
;;(prelude-require-package 'org-roam-bibtex)


(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; a few useful global keybindings for org-mode
;;(global-set-key "\C-cl" 'org-store-link)
;;(global-set-key "\C-ca" 'org-agenda)
;;(global-set-key "\C-cb" 'org-switchb)

(setq org-log-done t)

(defun prelude-org-mode-defaults ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (define-key newmap (kbd "C-a") 'org-beginning-of-line)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist))
  (prelude-org-babel)
  (prelude-org-roam)
  (org2ctex-toggle t)
  (org-bullets-mode 1)
  (org-indent-mode 1))

;;org babel
(defun prelude-org-babel ()
  (setq org-plantuml-jar-path
        (expand-file-name "~/.emacs.d/plantuml.jar"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; plantuml
     (plantuml . t)))
  (add-to-list 'org-babel-default-header-args:plantuml
               `(:cmdline . ,(concat "-charset utf-8"
                                    " "
                                    "-config"
                                    " "
                                    (expand-file-name "~/.emacs.d/pml/default.puml")))))

;;org roam
(setq org-roam-directory (expand-file-name "~/Documents/notes"))
(if (not (file-directory-p (expand-file-name "daily/" org-roam-directory)))
    (make-directory (expand-file-name "daily/" org-roam-directory)))
(setq org-roam-dailies-directory "daily/")
(setq org-roam-file-extensions '("org")
      org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

;; org agenda & org roam
;; https://magnus.therning.org/2021-03-14-keeping-todo-items-in-org-roam.html

(defun prelude-org-roam-todo-files ()
  "Return a list of note files containing todo tag."
  (seq-uniq
   (seq-map
    (lambda (node) (org-roam-node-file node))
    (cl-remove-if-not
     (lambda (node) (string-equal "TODO" (org-roam-node-todo node)))
     (org-roam-node-list)))))

(defun prelude-org-roam-update-todo-files (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (prelude-org-roam-todo-files)))

;; To ensure that the list of files with TODO items is kept up to date
;; when I open I also wrap org-agenda in an advice so
;; roam-extra:update-todo-files is called prior to the agenda being
;; opened.
(advice-add 'org-agenda :before #'prelude-org-roam-update-todo-files)

(defun prelude-org-roam()
  (add-hook 'after-save-hook (lambda ()
                               (org-roam-db-sync))))

;;ox-taskjuggler
(setq org-taskjuggler-default-reports
  '("textreport report \"Plan\" {
  formats html
  header '== %title =='
  center -8<-
    [#Plan Plan] | [#Resource_Allocation Resource Allocation]
    ----
    === Plan ===
    <[report id=\"plan\"]>
    ----
    === Resource Allocation ===
    <[report id=\"resourceGraph\"]>
  ->8-
}

# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
  headline \"Project Plan\"
  columns bsi, name {width 350}, start, end, effort, chart {scale week width 800}
  loadunit shortauto
  hideresource 1
}

# A graph showing resource allocation. It identifies whether each
# resource is under- or over-allocated for.
resourcereport resourceGraph \"\" {
  headline \"Resource Allocation Graph\"
  columns no, name, effort, annualleave, complete, weekly {width 700}
  loadunit shortauto
  hidetask ~(isleaf() & isleaf_())
  sorttasks plan.start.up
}")
)

;; (setq org-taskjuggler-default-global-properties
;; "
;; shift s40 \"Part time shift\" {
;;   workinghours wed, thu, fri off
;; }

;; leaves holiday \"National Day\" 2021-10-01 +5d,
;;        holiday \"Dragon Boat Festival\" 2021-06-12 +3d,
;;        holiday \"Mid-Autumn Festival\" 2021-09-19 +2d
;; ")
(setenv "LC_ALL" "zh_CN.UTF-8")
(setenv "LANG" "zh_CN.UTF-8")
(setenv "LANGUAGE" "zh_CN.UTF-8")
(setenv "LC_COLLATE" "zh_CN.UTF-8")
(setenv "LC_CTYPE" "zh_CN.UTF-8")
(setenv "LC_MESSAGES" "zh_CN.UTF-8")
(setenv "LC_MONETARY" "zh_CN.UTF-8")
(setenv "LC_NUMERIC" "zh_CN.UTF-8")
(setenv "LC_TIME" "zh_CN.UTF-8")
;; end

(setq prelude-org-mode-hook 'prelude-org-mode-defaults)

(add-hook 'org-mode-hook (lambda () (run-hooks 'prelude-org-mode-hook)))

(provide 'prelude-org)

;;; prelude-org.el ends here
