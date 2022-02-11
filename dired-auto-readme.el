;;; dired-auto-readme.el --- Auto-display README files in Dired biffers -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Arthur Miller

;; Author: Arthur Miller
;; Version: 1.0.0
;; Keywords: tools convenience
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/amno1/org-view-mode

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
;; All symbols starting with `dired-auto-readme--' are ment for internal use only.

;;; Code:

;;; Options

(defgroup dired-auto-readme nil
  "Automatically display 'readme' files when present in a dired buffer."
  :group 'files
  :prefix "dired-auto-readme")

(defcustom dired-auto-readme-display-images t
  "Display inline images in `org-mode'."
  :type 'boolean
  :group 'dired-auto-readme)

(defcustom dired-auto-readme-display-pretty-tables t
  "Display \"prettified\" tables in `org-mode'.
It only works in org-mode if you have org-pretty-table installed."
  :link "https://github.com/Fuco1/org-pretty-table"
  :type 'boolean
  :group 'dired-auto-readme)

(defcustom dired-auto-readme-assoc-alist
  '(("\\.org$" . dired-auto-readme-org)
    ("\\.txt$" . dired-auto-readme-txt)
    ("\\.\\(md\\|markdown\\)$" . dired-auto-readme-md))
  "List of regex to match against README file names and hooks to call on 
matching regex. Only first match is used, so you can push cons to the front to 
shadow the associations further in the list."
  :type 'list
  :group 'dired-auto-readme)

;;; Implementation

(require 'org)
(require 'wdired)
(require 'font-lock)
(require 'text-property-search)

(defvar dired-auto-readme-file "^\\([Rr]eadme\\|README\\)")

;; internal vars
(defvar-local dired-auto-readme--ext nil)
(defvar-local dired-auto-readme--inserted nil)
(defvar-local dired-auto-readme--readme-buff nil)

(defun dired-auto-readme--font-lock-fn (fl)
  (dired-auto-readme--fontify-buffer fl))
(defun dired-auto-readme--font-lock-fontify-buffer-fn ()
  (dired-auto-readme--fontify-buffer nil))
(defun dired-auto-readme--font-lock-ensure-fn (__beg __end)
  (font-lock-default-fontify-region
   (point-min) (dired-auto-readme--point) nil))
(defun dired-auto-readme--font-lock-fontify-region-fn (__beg __end &optional __v)
  (font-lock-default-fontify-region
   (point-min) (dired-auto-readme--point) nil))

(make-variable-buffer-local 'font-lock-function)
(make-variable-buffer-local 'font-lock-ensure-function)
(make-variable-buffer-local 'font-lock-fontify-region-function)
(make-variable-buffer-local 'font-lock-fontify-buffer-function)

(defun dired-auto-readme--dired-has-marks ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward dired-re-mark nil t)))

(defun dired-auto-readme--point ()
  (save-excursion
    (goto-char (point-min))
    (let ((dar (text-property-search-forward 'dar-point)))
      (if dar
          (prop-match-beginning dar)))))

(defun dired-auto-readme--fontify-buffer (fl)
  (let ((end (dired-auto-readme--point)))
    (unless end (setq end (point-max)))
    (font-lock-default-fontify-region (point-min) end fl)))

(defun dired-auto-readme--insert ()
  "Inserts content of Readme file in a current Dired buffer.

This function assumes the content is not currently inserted."
  (when dired-auto-readme--readme-buff
    (with-silent-modifications
      (save-excursion
        (buffer-disable-undo)
        (goto-char (1+ (dired-auto-readme--point)))
        (newline)
        (insert dired-auto-readme--readme-buff)
        (when (equal dired-auto-readme--ext "org")
          (add-to-invisibility-spec '(org-link))
          (when dired-auto-readme-display-images
            (org-display-inline-images t t)))
        ;; (when (equal dired-auto-readme--ext "md")
        ;;   (markdown-toggle-markup-hiding)
        ;;   (markdown-toggle-fontify-code-blocks-natively))
        (setq dired-auto-readme--inserted t)
        (buffer-enable-undo)))))

(defun dired-auto-readme--remove ()
  "Remove content of a Readme file from the current Dired buffer."
 (with-silent-modifications
   (save-excursion
     (buffer-disable-undo)
     (delete-region (1+ (dired-auto-readme--point)) (point-max))
     (setq dired-auto-readme--inserted nil)
     (buffer-enable-undo))))

(defun dired-auto-readme--before-readin ()
  (when dired-auto-readme--inserted
    (dired-auto-readme--remove)))

(defun dired-auto-readme--after-readin ()
  (when dired-auto-readme-mode
    (with-silent-modifications
      (save-excursion
        (buffer-disable-undo)
        (put-text-property (1- (point-max)) (point-max) 'dar-point 't)
        (font-lock-default-fontify-region (point-min) (dired-auto-readme--point) nil)
        (goto-char (point-max))
        (newline)
        (newline)
        (dired-auto-readme--insert))
      (buffer-enable-undo))))

(defun dired-auto-readme--to-dired ()
  (interactive)
  (wdired-change-to-dired-mode)
  (when dired-auto-readme-mode
    (dired-auto-readme--insert)))

(defun dired-auto-readme--to-wdired ()
  (interactive)
  (when dired-auto-readme--inserted
    (dired-auto-readme--remove))
  (wdired-change-to-wdired-mode))

(defun dired-auto-readme--enable ()
  "Insert README file in the current buffer."
  (add-hook 'dired-before-readin-hook #'dired-auto-readme--before-readin nil t)
  (add-hook 'dired-after-readin-hook #'dired-auto-readme--after-readin nil t)
  (font-lock-mode +1)
  (let ((file (car (directory-files "./" nil dired-auto-readme-file t))))
    (when file
      (setq font-lock-function #'dired-auto-readme--font-lock-fn
            font-lock-ensure-function #'dired-auto-readme--font-lock-ensure-fn
            font-lock-fontify-region-function #'dired-auto-readme--font-lock-fontify-region-fn
            font-lock-fontify-buffer-function #'dired-auto-readme--font-lock-fontify-buffer-fn)
      (setq dired-auto-readme--ext (file-name-extension file))
      (setq dired-auto-readme--readme-buff (dired-auto-readme--get-buffer file)))))

(defun dired-auto-readme--disable ()
  "Remove README file from the current dired buffer."
  (buffer-disable-undo)
  (when dired-auto-readme--inserted
    (with-silent-modifications
      (save-excursion
        (dired-auto-readme--remove)
        (let ((dar (dired-auto-readme--point)))
          (when dar
            (goto-char dar)
            (remove-text-properties (point) (1+ (point)) '(dar-point t)))))))
  (setq dired-auto-readme--ext nil
        dired-auto-readme--inserted nil
        dired-auto-readme--readme-buff nil)
  (remove-hook 'dired-before-readin-hook 'dired-auto-readme--before-readin t)
  (remove-hook 'dired-after-readin-hook 'dired-auto-readme--after-readin t)
  (setq font-lock-function #'font-lock-default-function
        font-lock-fontify-region-function #'font-lock-default-fontify-region
        font-lock-fontify-buffer-function #'font-lock-default-fontify-buffer)
  (font-lock-ensure)
  (buffer-enable-undo))

(defun dired-auto-readme--create-directory ()
  (interactive)
  (call-interactively 'dired-create-directory)
  (revert-buffer t t t))

(defun dired-auto-readme--create-empty-file ()
  (interactive)
  (call-interactively 'dired-create-directory)
  (revert-buffer t t t))

(defvar dired-auto-readme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap wdired-change-to-wdired-mode]
                #'dired-auto-readme--to-wdired)
    (define-key map [remap wdired-change-to-wdired-mode]
                #'dired-auto-readme--to-wdired)
    (define-key map [remap dired-create-directory]
                #'dired-auto-readme--create-directory)
    map)
  "Keymap used for dired-auto-readme-mode.")

(defun dired-auto-readme-md ()
  "Default function used to insert a README file in markdown format."
  (and (package-installed-p 'markdown-mode)
       (require 'markdown-mode)
       (markdown-view-mode))
  (font-lock-mode +1)
  (font-lock-ensure)
  (buffer-substring (point-min) (point-max)))

(defun dired-auto-readme-org ()
  "Default function used to insert a README file in org-mode."
  (require 'org)
  (call-interactively 'org-mode)
  (run-hooks 'org-mode-hook)
  (font-lock-mode +1)
  (font-lock-ensure)
  (org-toggle-pretty-entities)
  (setq org-link-descriptive t)
  (when (and dired-auto-readme-display-pretty-tables
             (require 'org-pretty-table))
    (org-pretty-table-mode 1))
  (require 'package)
  (and (package-installed-p 'org-view-mode)
       (require 'org-view-mode)
       (org-view-mode +1)))

(defun dired-auto-readme-txt ()
  "Default function used to insert a README file in text format.

Really, does nothing, text is just inserted as-is."
  (ignore))

(defun dired-auto-readme--get-buffer (file)
  "Internal function that actually does the work."
  (with-silent-modifications
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (catch 'done
        (dolist (elt dired-auto-readme-assoc-alist)
          (when (string-match-p (car elt) file)
            (funcall (cdr elt))
            (throw 'done t))))
      (goto-char (point-min))
      (while (not (eobp))
        (insert "  ")
        (forward-line))
      (buffer-substring (point-min) (point-max)))))

;;; User commands

;;;###autoload
(define-minor-mode dired-auto-readme-mode
  "Dired minor mode to preview README file in current directory."
  :global nil :lighter " README"
  (dired-auto-readme--disable)
  (if dired-auto-readme-mode
      (dired-auto-readme--enable)
    (dired-auto-readme--disable)))

(provide 'dired-auto-readme)

;;; dired-auto-readme.el ends here
