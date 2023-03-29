;;; dired-auto-readme.el --- Auto-display README file in Dired biffers -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Arthur Miller

;; Author: Arthur Miller
;; Version: 1.0.0
;; Keywords: tools convenience
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/amno1/dired-auto-readme

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
;; Display README files in Dired buffers when such are present in a manner
;; similar to public forges such as Codeberg or Github/Gitlab.  README files are
;; displayed by default as plain text buffers, but if you have additional
;; packages such as markdown-mode or org-view-mode installed, you can enable
;; those for prettier previews.
;;

;;; Code:

;; All symbols starting with `dired-auto-readme--' are ment for internal use only.

;;; User Options
(defgroup dired-auto-readme nil
  "Automatically display 'readme' files when present in a Dired buffer."
  :group 'files
  :prefix "dired-auto-readme")

;; need this because directory-files cares about case 
(defcustom dired-auto-readme-file "^\\([Rr]eadme\\|README\\)"
  "Regex used to tell which file is considered a readme file."
  :type 'string
  :group 'dired-auto-readme)

(defcustom dired-auto-readme-alist nil
  "List of modes and custom hooks to call when a README buffer is read-in.
These hooks are called after the major mode is set and font-lock is enabled."
  :type 'list
  :group 'dired-auto-readme)

;;; Implementation
(require 'font-lock)
(require 'text-property-search)
(eval-when-compile (require 'wdired))

(defvar-local dired-auto-readme--text nil)

(defmacro with-silent-buffer (&rest body)
  "Like `with-silent-modifications' but with `save-excursion' and restore undo."
  (declare (debug t) (indent 0))
  (let ((modified (make-symbol "modified")))
    `(let* ((,modified (buffer-modified-p))
            (undo-list buffer-undo-list)
            (inhibit-read-only t)
            (inhibit-modification-hooks t))
       (unwind-protect
           (progn
             (save-excursion
               ,@body))
         (when (or (not ,modified)
                   (eq ,modified 'autosaved))
           (setq buffer-undo-list undo-list)
           (restore-buffer-modified-p ,modified))))))

(defun dired-auto-readme--point ()
  "Return point of readme-file insertion or end of dired-buffer."
  (let ((dar (or (text-property-search-backward 'dar-point)
                 (text-property-search-forward 'dar-point))))
    (if dar (prop-match-beginning dar) (point-max))))

(defun dired-auto-readme--fontify-buffer (_ _ &optional _)
  "Fontify Dired portion of the buffer."
  (font-lock-default-fontify-region 1 (dired-auto-readme--point) nil))

(defun dired-auto-readme--insert (&optional _)
  "Insert content of Readme file in a current Dired buffer.

This function assumes the content is not currently inserted."
  (with-silent-buffer
    (goto-char (point-max))
    (insert dired-auto-readme--text)))

(defun dired-auto-readme--remove (&optional _)
  "Remove content of a Readme file from the current Dired buffer."
  (with-silent-buffer
    (let ((dar (dired-auto-readme--point)))
      (when dar (delete-region dar (point-max))))))

(defun dired-auto-readme--enable ()
  "Insert README file in the current buffer."
  (let* ((files (directory-files "./" nil dired-auto-readme-file t))
         (files (cl-remove-if #'file-directory-p files))
         (file (car files)))
    (when file
      (add-hook 'dired-after-readin-hook #'dired-auto-readme--insert nil t)
      (add-hook 'dired-before-readin-hook #'dired-auto-readme--remove nil t)
      (advice-add 'wdired-change-to-dired-mode :after #'dired-auto-readme--insert)
      (advice-add 'wdired-change-to-wdired-mode :before #'dired-auto-readme--remove)
      (advice-add 'dired-create-directory :after #'dired-auto-readme--insert)
      (advice-add 'dired-create-directory :before #'dired-auto-readme--remove)
      (advice-add 'dired-create-empty-file :after #'dired-auto-readme--insert)
      (advice-add 'dired-create-empty-file :before #'dired-auto-readme--remove)
      (setq dired-auto-readme--text (dired-auto-readme--text file)
            font-lock-fontify-region-function #'dired-auto-readme--fontify-buffer))
    (revert-buffer t t)))

(defun dired-auto-readme--disable ()
  "Remove README file from the current Dired buffer."
  (with-silent-buffer
    (when dired-auto-readme--text
      (dired-auto-readme--remove)
      (remove-hook 'dired-after-readin-hook #'dired-auto-readme--insert t)
      (remove-hook 'dired-before-readin-hook #'dired-auto-readme--remove t)
      (advice-remove 'dired-create-directory #'dired-auto-readme--insert)
      (advice-remove 'dired-create-directory #'dired-auto-readme--remove)
      (advice-remove 'dired-create-empty-file #'dired-auto-readme--insert)
      (advice-remove 'dired-create-empty-file #'dired-auto-readme--remove)
      (advice-remove 'wdired-change-to-dired-mode #'dired-auto-readme--insert)
      (advice-remove 'wdired-change-to-wdired-mode #'dired-auto-readme--remove)
      (setq dired-auto-readme--text nil
            font-lock-fontify-region-function #'font-lock-default-fontify-region)))
  (revert-buffer t t))

(defun dired-auto-readme--text (file)
  "Internal function that actually does the work.
Argument FILE Readme file to insert."
  (with-temp-buffer
    (with-silent-modifications
      (let ((buffer-file-name file))
        (insert-file-contents file)
        (set-auto-mode)
        (let ((hook (cdr (assoc major-mode dired-auto-readme-alist))))
          (when hook (funcall hook)))
        (font-lock-mode)
        (font-lock-ensure)
        (goto-char 1)
        ;; put some space from the last file
        (insert "\n\n")
        (goto-char 1)
        (put-text-property 1 2 'dar-point t)
        ;; insert two spaces to align to text in dired-mode
        (while (not (eobp)) (insert "  ") (forward-line))
        (buffer-string)))))

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
