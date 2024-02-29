;;; dired-auto-readme.el --- Auto-display README file in Dired biffers -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Arthur Miller

;; Author: Arthur Miller
;; Version: 1.0.0
;; Keywords: tools convenience
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.5"))
;; URL: https://github.com/amno1/dired-auto-readme

;;; Licence
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

;; Display README files in Dired buffers when such are present in a manner
;; similar to public forges such as Codeberg/Github/Gitlab.  README files are
;; displayed by default as plain text buffers, but if you have additional
;; packages such as markdown-mode or org-view-mode installed, you can enable
;; those for prettier previews.

;;; Code:
(require 'text-property-search)


;;; User Options

(defgroup dired-auto-readme nil
  "Automatically display 'readme' files when present in a Dired buffer."
  :group 'files
  :prefix "dired-auto-readme")

(defcustom dired-auto-readme-files '("readme.md"
                                     "readme.org"
                                     "readme.rst"
                                     "readme.markdown"
                                     "readme"
                                     "manifest")
  "A list of regular expressions used to tell which file to use."
  :type '(list string)
  :group 'dired-auto-readme)

(defcustom dired-auto-readme-alist
  '((org-mode . dired-auto-readme-fake-org))
  "List of modes and custom hooks to call when a README buffer is read-in.
The hook is called after the text has been inserted in Dired buffer."
  :type 'alist)

;;; Implementation

(defun dired-auto-readme-fake-org ()
  "Hack to display descriptive links in Dired buffer."
  (org-fold-initialize "..."))

(defun dired-auto-readme--point ()
  "Return point of readme-file insertion or end of dired-buffer."
  (if-let ((dar (or (text-property-search-backward 'bis)
                    (text-property-search-forward 'bis))))
      (prop-match-beginning dar) (point-max)))

(defun dired-auto-readme--fontify-region (_ _ &optional v)
  "Fontify Dired portion of the buffer."
  (font-lock-default-fontify-region 1 (dired-auto-readme--point) v))

(defun dired-auto-readme--find-file ()
  "Return first file-name in a `current-buffer' matching REGEX."
  (goto-char (point-min))
  (catch 'file
    (save-excursion
      (while (dired-next-line 1)
        (let ((file (dired-file-name-at-point))
              (case-fold-search t))
          (unless (file-directory-p file)
            (dolist (rg dired-auto-readme-files)
              (when (looking-at-p rg)
                (throw 'file file)))))))))

(defun dired-auto-readme--insert (&optional _)
  "Insert content of Readme file in a current Dired buffer.

This function assumes the content is not currently inserted."
  (when-let* ((file (dired-auto-readme--find-file)))
    (with-silent-modifications
      (save-excursion
        (setq-local font-lock-fontify-region-function
                    #'dired-auto-readme--fontify-region)
        (let* ((enable-local-variables nil)
               (data (dired-auto-readme--read-in file))
               (action (assoc (cdr data) dired-auto-readme-alist)))
          (goto-char (point-max))
          (insert (car data))
          (when action
            (funcall (cdr action))))))))

(defun dired-auto-readme--remove (&optional _)
  "Remove content of a Readme file from the current Dired buffer."
  (with-silent-modifications
    (save-excursion
      ;; invisibility spec is left with some garbage; fix for another day
      (kill-local-variable 'font-lock-fontify-region-function)
      (when-let ((dar (dired-auto-readme--point)))
        (delete-region dar (point-max))))))

(defun dired-auto-readme--enable ()
  "Insert README file in the current buffer."
  (add-hook 'dired-after-readin-hook #'dired-auto-readme--insert nil t)
  (add-hook 'dired-before-readin-hook #'dired-auto-readme--remove nil t)
  (advice-add 'wdired-change-to-dired-mode :after #'dired-auto-readme--insert)
  (advice-add 'wdired-change-to-wdired-mode :before #'dired-auto-readme--remove)
  (advice-add 'dired-create-directory :after #'dired-auto-readme--insert)
  (advice-add 'dired-create-directory :before #'dired-auto-readme--remove)
  (advice-add 'dired-create-empty-file :after #'dired-auto-readme--insert)
  (advice-add 'dired-create-empty-file :before #'dired-auto-readme--remove)
  (when (eq major-mode 'dired-mode)
    (with-silent-modifications
      (dired-auto-readme--insert)
      (dired-revert t t))))

(defun dired-auto-readme--disable ()
  "Remove README file from the current Dired buffer."
  (remove-hook 'dired-after-readin-hook #'dired-auto-readme--insert t)
  (remove-hook 'dired-before-readin-hook #'dired-auto-readme--remove t)
  (advice-remove 'dired-create-directory #'dired-auto-readme--insert)
  (advice-remove 'dired-create-directory #'dired-auto-readme--remove)
  (advice-remove 'dired-create-empty-file #'dired-auto-readme--insert)
  (advice-remove 'dired-create-empty-file #'dired-auto-readme--remove)
  (advice-remove 'wdired-change-to-dired-mode #'dired-auto-readme--insert)
  (advice-remove 'wdired-change-to-wdired-mode #'dired-auto-readme--remove)
  (and (eq major-mode 'dired-mode)
    (with-silent-modifications
      (dired-auto-readme--remove)
      (dired-revert t t))))

(defun dired-auto-readme--read-in (file)
  "Internal function that actually does the work.
Argument FILE Readme file to insert."
  (with-temp-buffer
    (let ((buffer-file-name file))
      (insert "\n") ; put some space from the dired last file
      (insert-file-contents file)
      (set-auto-mode)
      (when (eq major-mode 'markdown-mode)
        (gfm-view-mode))
      (font-lock-mode)
      (font-lock-ensure)
      (goto-char 1)
      (put-text-property
       1 2 'bis (if (listp buffer-invisibility-spec)
                    (copy-sequence buffer-invisibility-spec)
                  't))
      ;; insert two spaces to align to text in dired-mode
      (while (not (eobp)) (insert "  ") (forward-line))
      (cons (buffer-string) major-mode))))

;;; User commands
;;;###autoload
(define-minor-mode dired-auto-readme-mode
  "Dired minor mode to enable README file preview in current directory."
  :global nil :lighter " README"
  (if dired-auto-readme-mode
      (dired-auto-readme--enable)
    (dired-auto-readme--disable)))

(provide 'dired-auto-readme)
;;; dired-auto-readme.el ends here
