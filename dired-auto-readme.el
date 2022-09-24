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

;; internal vars
(defvar-local dired-auto-readme--inserted nil)
(defvar-local dired-auto-readme--readme-buff nil)

(defun dired-auto-readme--font-lock-fn ()
  "Dired-auto-readme `font-lock-fn'.
Argument FL Same as in `font-lock-fontify-buffer' function."
  (dired-auto-readme--fontify-buffer))

(defun dired-auto-readme--font-lock-fontify-buffer-fn ()
  "Dired-auto-readme `font-lock-fontify-buffer-function'."
  (dired-auto-readme--fontify-buffer))

(defun dired-auto-readme--font-lock-ensure-fn (beg end)
  "Dired-auto-readme `font-lock-ensure-fn'.
Argument BEG Start of font-lock region.
Argument END End of font-lock region."
  (let* ((dar (dired-auto-readme--point))
         (beg (if (< beg dar) beg 1))
         (end (if (< end dar) end dar)))
    (font-lock-default-fontify-region beg end nil)))

(defun dired-auto-readme--font-lock-fontify-region-fn (beg end &optional _v)
  "Dired-auto-readme `font-lock-fontify-region-function'.
Argument BEG Start of font-lock region.
Argument END End of font-lock region."
  (let* ((dar (dired-auto-readme--point))
         (beg (if (< beg dar) beg 1))
         (end (if (< end dar) end dar)))
  (font-lock-default-fontify-region beg end nil)))

(defun dired-auto-readme--point ()
  "Return insertion point of README buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((dar (text-property-search-forward 'dar-point)))
      (if dar (prop-match-beginning dar)))))

(defun dired-auto-readme--fontify-buffer ()
  "Fontify Dired portion of the buffer."
  (font-lock-default-fontify-region
   1 (or (dired-auto-readme--point) (point-max)) nil))

(defun dired-auto-readme--insert ()
  "Insert content of Readme file in a current Dired buffer.

This function assumes the content is not currently inserted."
  (when dired-auto-readme--readme-buff
    (with-silent-modifications
      (save-excursion
        (let ((undo-list buffer-undo-list))
          (goto-char (1+ (dired-auto-readme--point)))
          (newline)
          (insert dired-auto-readme--readme-buff)
          (setq dired-auto-readme--inserted t
                buffer-undo-list undo-list))))))

(defun dired-auto-readme--remove ()
  "Remove content of a Readme file from the current Dired buffer."
 (with-silent-modifications
   (save-excursion
     (let ((undo-list buffer-undo-list))
       (delete-region (1+ (dired-auto-readme--point)) (point-max))
       (setq dired-auto-readme--inserted nil
             buffer-undo-list undo-list)))))

(defun dired-auto-readme--before-readin ()
  "Dired-auto-reamde hook called before README file is read in."
  (when dired-auto-readme--inserted
    (dired-auto-readme--remove)))

(defun dired-auto-readme--after-readin ()
  "Dired-auto-reamde hook called after README files is read in."
  (when (bound-and-true-p dired-auto-readme-mode)
    (with-silent-modifications
      (let ((undo-list buffer-undo-list))
        (save-excursion
         (put-text-property (1- (point-max)) (point-max) 'dar-point 't)
         (font-lock-default-fontify-region (point-min) (dired-auto-readme--point) nil)
         (goto-char (point-max))
         (newline)
         (dired-auto-readme--insert))
        (setq buffer-undo-list undo-list)))))

(defun dired-auto-readme--enable ()
  "Insert README file in the current buffer."
  (add-hook 'dired-before-readin-hook #'dired-auto-readme--before-readin nil t)
  (add-hook 'dired-after-readin-hook #'dired-auto-readme--after-readin nil t)
  (font-lock-mode +1)
  (let ((file (car (directory-files "./" nil dired-auto-readme-file t))))
    (when file
      (setq-local
       font-lock-function #'dired-auto-readme--font-lock-fn
       font-lock-ensure-function #'dired-auto-readme--font-lock-ensure-fn
       font-lock-fontify-region-function #'dired-auto-readme--font-lock-fontify-region-fn
       font-lock-fontify-buffer-function #'dired-auto-readme--font-lock-fontify-buffer-fn)
      (setq dired-auto-readme--readme-buff (dired-auto-readme--get-buffer file)))))

(defun dired-auto-readme--disable ()
  "Remove README file from the current Dired buffer."
  (let ((undo-list buffer-undo-list))
    (when dired-auto-readme--inserted
     (with-silent-modifications
       (save-excursion
         (dired-auto-readme--remove)
         (let ((dar (dired-auto-readme--point)))
           (when dar
             (goto-char dar)
             (remove-text-properties (point) (1+ (point)) '(dar-point t)))))
       (setq buffer-undo-list undo-list))))
  (setq dired-auto-readme--inserted nil
        dired-auto-readme--readme-buff nil)
  (remove-hook 'dired-before-readin-hook 'dired-auto-readme--before-readin t)
  (remove-hook 'dired-after-readin-hook 'dired-auto-readme--after-readin t)
  (setq font-lock-function #'font-lock-default-function
        font-lock-fontify-region-function #'font-lock-default-fontify-region
        font-lock-fontify-buffer-function #'font-lock-default-fontify-buffer)
  (if (bound-and-true-p font-lock-mode) (font-lock-ensure)))

(defvar dired-auto-readme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap wdired-change-to-wdired-mode]
                #'dired-auto-readme--to-wdired)
    (define-key map [remap wdired-change-to-wdired-mode]
                #'dired-auto-readme--to-wdired)
    (define-key map [remap dired-create-directory]
                #'dired-auto-readme--create-directory)
    (define-key map [remap dired-create-empty-file]
                #'dired-auto-readme--create-empty-file)
    map)
  "Keymap used for `dired-auto-readme-mode'.")

;; (defun dired-auto-readme--get-overlays ()
;;   "Copy overlays from org-mode buffer to dired buffer."
;;   (let ((overlays))
;;     (mapc (lambda (ov)
;;             (unless (memq (overlay-get ov 'face) '(region hl-line))
;;               (push ov overlays)))
;;           (overlays-in 1 (point-max)))
;;     overlays))

(defun dired-auto-readme--get-buffer (file)
  "Internal function that actually does the work.
Argument FILE Readme file to insert."
  (with-silent-modifications
    (with-temp-buffer
      (let ((buffer-file-name file))
        (insert-file-contents file)
        (set-auto-mode))
      (let ((hook (assoc major-mode dired-auto-readme-alist)))
        (funcall (or (cdr hook) #'ignore)))
      ;; insert two spaces to align to text in dired-mode
      (goto-char (point-min))
      (while (not (eobp)) (insert "  ") (forward-line))
      ;; copy potiential overlays that some mode can introduce (not impl. yet)
      ;; (put-text-property 1 2 'dar-overlays (dired-auto-readme--get-overlays))
      (buffer-string))))

;;; User commands
(defun dired-auto-readme--create-directory ()
  "Same as `dired-create-directory' adapted for `dired-auto-readme-mode'."
  (interactive)
  (call-interactively 'dired-create-directory)
  (revert-buffer t t t))

(defun dired-auto-readme--create-empty-file ()
  "Same as `dired-create-empty-file' adapted for `dired-auto-readme-mode'."
  (interactive)
  (call-interactively 'dired-create-empty-file)
  (revert-buffer t t t))

(defun dired-auto-readme--to-dired ()
  "Switch from wdired to Dired mode when `dired-auto-readme-mode' is enabled."
  (interactive)
  (wdired-change-to-dired-mode)
  (when (bound-and-true-p dired-auto-readme-mode)
    (dired-auto-readme--insert)))

(defun dired-auto-readme--to-wdired ()
  "Switch to wdired mode when `dired-auto-readme-mode' is enabled."
  (interactive)
  (when dired-auto-readme--inserted
    (dired-auto-readme--remove))
  (wdired-change-to-wdired-mode))

;;;###autoload
(define-minor-mode dired-auto-readme-mode
  "Dired minor mode to enable README file preview in current directory."
  :global nil :lighter " README"
  (if dired-auto-readme-mode
      (dired-auto-readme--enable)
    (dired-auto-readme--disable)))

(provide 'dired-auto-readme)

;;; dired-auto-readme.el ends here
