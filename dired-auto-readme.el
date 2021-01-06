;; dired-auto-readme.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; All symbols starting with `dar--' are ment for internal use only.

;;; Code:

(defgroup dired-auto-readme nil
  "Automatically display 'readme' files when present in a dired buffer."
  :group 'files
  :prefix "dired-auto-readme")

(defcustom dired-auto-readme-display-images t
  "Display inline images in `org-mode'."
  :type 'boolean)

(defcustom dired-auto-readme-display-pretty-tables t
  "Display \"prettified\" tables in `org-mode'.
It only works in org-mode if you have org-pretty-table installed."
  :link "https://github.com/Fuco1/org-pretty-table"
  :type 'boolean)

(defcustom dired-auto-readme-files '("README"
                                     "README.md"
                                     "README.org"
                                     "README.txt"
                                     "README.markdown"
                                     "readme"
                                     "readme.txt"
                                     "readme.md"
                                     "readme.org"
                                     "readme.markdown"
                                     "Readme"
                                     "Readme.txt"
                                     "Readme.md"
                                     "Readme.org"
                                     "Readme.markdown")
  
  "List of file names to display automatically.
First file in the
directory matching the list will be used."   :type 'list)

;; internal vars
(defvar dar--inserted)
(defvar dar--orig-buff)
(defvar dar--start-point)
(make-variable-buffer-local 'dar--inserted)
(make-variable-buffer-local 'dar--orig-buff)
(make-variable-buffer-local 'dar--start-point)

(defun dar--insert ()
  "Insert README file in the current buffer."
  (setq inhibit-read-only t)
  (catch 'break
    (dolist (file dired-auto-readme-files)
      (when (file-exists-p file)
        (save-excursion
          (let (text (ext (file-name-extension file)))
            (with-current-buffer (get-buffer-create file)
              (insert-file-contents file)
              (cond ((or (equal ext "md")
                         (equal ext "markdown"))
                     (when (require 'markdown-mode)
                       (call-interactively 'markdown-mode)))
                    ((equal ext "org")
                     (when (require 'org)
                       (goto-char (point-min))
                       (call-interactively 'org-mode))))
              (font-lock-ensure)
              (goto-char (point-min))
              (while (not (eobp))
                (insert "  ")
                (forward-line 1))
              (setq text (buffer-substring (point-min) (point-max)))
              (kill-buffer))
            (setq dar--orig-buff (buffer-substring (point-min) (point-max)))
            (font-lock-mode -1)
            (delete-region (point-min) (point-max))
            (insert dar--orig-buff)
            (setq dar--start-point (point))
            (newline)
            (insert text)
            (setq dar--inserted t)
            (when (equal ext "org")
              (org-toggle-link-display)
              (org-toggle-link-display)
              (org-toggle-pretty-entities)
              (when dired-auto-readme-display-images
                (org-display-inline-images t t))
              (when (and dired-auto-readme-display-pretty-tables
                         (require 'org-pretty-table))
                (org-pretty-table-mode 1)))
            (font-lock-fontify-region 1 dar--start-point))
          (setq inhibit-read-only nil)
          (throw 'break t))))))

(defun dar--remove ()
  "Remove README file from the current dired buffer."
  (setq inhibit-read-only t)
  (save-excursion
    (delete-region 1 (point-max))
    (insert dar--orig-buff)
    (kill-local-variable 'dar--inserted)
    (kill-local-variable 'dar--orig-buff)
    (kill-local-variable 'dar--start-point)
    (font-lock-mode +1))
  (setq inhibit-read-only nil))

;;;###autoload
(define-minor-mode dired-auto-readme-mode
  "Dired minor mode to preview README in current directory."
  :global nil :lighter " README"
  (when (derived-mode-p 'dired-mode)
    (cond (dired-auto-readme-mode
           (unless dar--inserted
             (dar--insert))
           (add-hook 'dired-after-readin-hook
                     'dar--insert nil t)
           (add-hook 'dired-mode-hook 'dired-auto-readme-mode))
          (t
           (when dar--inserted
             (dar--remove))
           (remove-hook 'dired-after-readin-hook
                        'dar--insert t)
           (remove-hook 'dired-mode-hook
                        'dired-auto-readme-mode)))))

(provide 'dired-auto-readme)

;;; dired-auto-readme.el ends here
