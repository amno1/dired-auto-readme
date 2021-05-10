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

(require 'org)
(require 'wdired)
(require 'font-lock)
(require 'text-property-search)

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

(defvar dired-auto-readme-file "^\\([Rr]eadme\\|README\\)")
(defvar dired-auto-readme-alist nil)
;;(defvar font-lock-default-ensure-fn #'font-lock-default-ensure-function)

;; internal vars
(defvar-local dar--ext nil)
(defvar-local dar--inserted nil)
(defvar-local dar--readme-buff nil)

;; font-lock please do nothing untill asked!
(defun dar--font-lock-fn (fl)
  (dar--fontify-buffer fl))
(defun dar--font-lock-fontify-buffer-fn ()
  (dar--fontify-buffer nil))
(defun dar--font-lock-ensure-fn (__beg __end)
  (font-lock-default-fontify-region (point-min) (dar--point) nil))
(defun dar--font-lock-fontify-region-fn (__beg __end &optional __v)
  (font-lock-default-fontify-region (point-min) (dar--point) nil))

(make-variable-buffer-local 'font-lock-function)
(make-variable-buffer-local 'font-lock-ensure-function)
(make-variable-buffer-local 'font-lock-fontify-region-function)
(make-variable-buffer-local 'font-lock-fontify-buffer-function)

(defmacro cvg (&rest var-list)
  `(let ((vars '(,@var-list)))
     (dolist (var vars)
       (set var nil))))

(defmacro cvl (&rest var-list)
  `(let ((vars '(,@var-list)))
     (dolist (var vars)
       (setq var nil))))

(defun dar--dired-has-marks ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward dired-re-mark nil t)))

(defun dar--point ()
  (save-excursion
    (goto-char (point-min))
    (let ((dar (text-property-search-forward 'dar-point)))
      (if dar
          (prop-match-beginning dar)))))

(defun dar--fontify-buffer (fl)
  (let ((end (dar--point)))
    (unless end (setq end (point-max)))
    (font-lock-default-fontify-region (point-min) end fl)))

(defun dar--insert ()
  "Inserts content of Readme file in a current Dired buffer.  Assumes the
content is not currently inserted."
  (when dar--readme-buff
    (with-silent-modifications
      (save-excursion
        (buffer-disable-undo)
        (goto-char (1+ (dar--point)))
        (newline)
        (insert dar--readme-buff)
        (when (equal dar--ext "org")
          (add-to-invisibility-spec '(org-link))
          (when dired-auto-readme-display-images
            (org-display-inline-images t t)))
        (setq dar--inserted t)
        (buffer-enable-undo)))))

(defun dar--remove ()
  "Remove content of a Readme file from the current Dired buffer.  "
 (with-silent-modifications
   (save-excursion
     (buffer-disable-undo)
     (delete-region (1+ (dar--point)) (point-max))
     (cvg dar--inserted)
     (buffer-enable-undo))))

(defun dar--before-readin ()
  (when dar--inserted
    (dar--remove)))

(defun dar--after-readin ()
  (when dired-auto-readme-mode
    (with-silent-modifications
      (save-excursion
        (buffer-disable-undo)
        (put-text-property (1- (point-max)) (point-max) 'dar-point 't)
        (font-lock-default-fontify-region (point-min) (dar--point) nil)
        (goto-char (point-max))
        (newline)
        (newline)
        (dar--insert))
      (buffer-enable-undo))))

(defun dar--to-dired ()
  (interactive)
  (wdired-change-to-dired-mode)
  (when dired-auto-readme-mode
    (dar--insert)))

(defun dar--to-wdired ()
  (interactive)
  (when dar--inserted
    (dar--remove))
  (wdired-change-to-wdired-mode))

(defun dar--enable ()
  "Insert README file in the current buffer."
  (add-hook 'dired-before-readin-hook #'dar--before-readin nil t)
  (add-hook 'dired-after-readin-hook #'dar--after-readin nil t)
  (font-lock-mode +1)
  (let ((file (car (directory-files "./" nil dired-auto-readme-file t))))
    (when file
      (setq font-lock-function #'dar--font-lock-fn
            font-lock-ensure-function #'dar--font-lock-ensure-fn
            font-lock-fontify-region-function #'dar--font-lock-fontify-region-fn
            font-lock-fontify-buffer-function #'dar--font-lock-fontify-buffer-fn)
      (setq dar--ext (file-name-extension file))
      (setq dar--readme-buff
            (cond ((or (equal dar--ext "md")
                       (equal dar--ext "markdown"))
                   (dar--open-md file))
                  ((equal dar--ext "org")
                   (dar--open-org file))
                  (t (dar--open-txt file)))))))

(defun dar--disable ()
  "Remove README file from the current dired buffer."
  (buffer-disable-undo)
  (when dar--inserted
    (with-silent-modifications
      (save-excursion
        (dar--remove)
        (let ((dar (dar--point)))
          (when dar
            (goto-char dar)
            (remove-text-properties (point) (1+ (point)) '(dar-point t)))))))
  (cvl dar--inserted dar--readme-buff dar--ext)
  (remove-hook 'dired-before-readin-hook 'dar--before-readin t)
  (remove-hook 'dired-after-readin-hook 'dar--after-readin t)
  (setq font-lock-function #'font-lock-default-function
        font-lock-fontify-region-function #'font-lock-default-fontify-region
        font-lock-fontify-buffer-function #'font-lock-default-fontify-buffer)
  (font-lock-ensure)
  (buffer-enable-undo))

(defun dar--create-directory ()
  (interactive)
  (call-interactively 'dired-create-directory)
  (revert-buffer t t t))

(defun dar--create-empty-file ()
  (interactive)
  (call-interactively 'dired-create-directory)
  (revert-buffer t t t))

(defvar dired-auto-readme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap wdired-change-to-wdired-mode] 'dar--to-wdired)
    (define-key map [remap wdired-change-to-wdired-mode] 'dar--to-wdired)
    (define-key map [remap dired-create-directory] 'dar--create-directory)
    map)
  "Keymap used for dired-auto-readme-mode.")

;;;###autoload
(define-minor-mode dired-auto-readme-mode
  "Dired minor mode to preview README in current directory."
  :global nil :lighter " README"
  (dar--disable)
  (if dired-auto-readme-mode
      (dar--enable)
    (dar--disable)))

(defun dar--open-md (file)
  "Insert a markdown Readme file."
  (with-temp-buffer
    (insert-file-contents file)
    (when (require 'markdown-mode)
      (markdown-view-mode))
    (font-lock-mode +1)
    (font-lock-ensure)
      (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (not (eobp))
        (insert "  ")
        (forward-line 1)))
    (format "%s" (buffer-substring (point-min) (point-max)))))

(defun dar--open-org (file)
  "Insert a Readme file written as org-mode."
  (with-temp-buffer
    (insert-file-contents file)
    (when (require 'org)
      (call-interactively 'org-mode)
      (font-lock-mode +1)
      (run-hooks 'org-mode-hook)
      (org-toggle-pretty-entities)
      (font-lock-ensure)
      (setq org-link-descriptive t)
      (when (and dired-auto-readme-display-pretty-tables
                 (require 'org-pretty-table))
        (org-pretty-table-mode 1)))
    (goto-char (point-min))
    (while (not (eobp))
      (insert "  ")
      (forward-line 1))
    (format "%s" (buffer-substring (point-min) (point-max)))))

(defun dar--open-txt (file)
  "Insert a plain textual Readme file."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (not (eobp))
      (insert "  ")
      (forward-line 1))
    (call-interactively 'text-mode)
    (font-lock-mode t)
    (font-lock-ensure)
    (format "%s" (buffer-substring (point-min) (point-max)))))

(provide 'dired-auto-readme)

;;; dired-auto-readme.el ends here
