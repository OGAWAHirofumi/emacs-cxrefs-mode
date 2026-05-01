;;; cxrefs-capf.el --- cxrefs completion at point function  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  OGAWA Hirofumi

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Keywords: tools

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

;; Example config.
;;
;;   (add-hook 'completion-at-point-functions #'cxrefs-capf nil t)
;;

;;; Code:

(require 'cxrefs)

(defgroup cxrefs-capf nil
  "Provide capf based on cxrefs."
  :version "25.1"
  :group 'cxrefs)

(defcustom cxrefs-capf-shutdown-secs 1800
  "Shutdown context automatically after this seconds.
If 0, disable auto shutdown."
  :type 'integer)

(defvar cxrefs-capf-ctx nil)
(defvar cxrefs-capf-ctx-timer nil)

(defun cxrefs-capf--cancel-timer ()
  "Cancel shutdown timer."
  (when cxrefs-capf-ctx-timer
    (cancel-timer cxrefs-capf-ctx-timer)
    (setq cxrefs-capf-ctx-timer nil)))

(defun cxrefs-capf--shutdown-timer ()
  "Set timer to shutdown context automatically."
  (cxrefs-capf--cancel-timer)
  (when (> cxrefs-capf-shutdown-secs 0)
    (setq cxrefs-capf-ctx-timer
          (run-with-timer cxrefs-capf-shutdown-secs nil
                          (lambda ()
                            (with-local-quit
                              (cxrefs-capf-ctx-quit)))))))

(defun cxrefs-capf-ctx-quit ()
  "Quit current capf context."
  (interactive)
  (cxrefs-capf--cancel-timer)
  (when cxrefs-capf-ctx
    (cxrefs-backend-command cxrefs-capf-ctx 'quit)
    (setq cxrefs-capf-ctx nil)))

(defun cxrefs-capf--find-ctx ()
  "Find or build context."
  (if-let* ((dir (locate-dominating-file default-directory "cscope.out"))
            (dir (expand-file-name dir)))
      (unless (and cxrefs-capf-ctx
                   (string= dir (cxrefs-ctx-dir cxrefs-capf-ctx)))
        (cxrefs-capf-ctx-quit)
        (setq cxrefs-capf-ctx (cxrefs-ctx-make dir))
        (cxrefs-check-and-build-db cxrefs-capf-ctx))
    (cxrefs-capf-ctx-quit))
  (when cxrefs-capf-ctx
    (cxrefs-capf--shutdown-timer))
  cxrefs-capf-ctx)

(defun cxrefs-capf--get-ctx ()
  "Find context."
  (or (cxrefs-context-current)
      (cxrefs-capf--find-ctx)))

(defun cxrefs-capf--lookup (prefix)
  "Lookup PREFIX by calling `cxrefs-xref-command'."
  (when-let* ((ctx (cxrefs-capf--get-ctx))
              (query (concat "^" prefix "[a-zA-Z0-9_]*"))
              (locs (cxrefs-xref-command ctx 'define query nil nil)))
    (let (result)
      (dolist (loc locs result)
        (let ((func (cxrefs-location-func loc))
              (file (cxrefs-location-file loc))
              (linum (cxrefs-location-line loc))
              (hint (cxrefs-location-hint loc)))
          (unless (string= func "<global>")
            (push (propertize func
                              'meta hint
                              'location (cons file linum))
                  result))))
      (nreverse result))))

(defun cxrefs-capf--annotation (arg)
  "Return annotation string for ARG."
  (when arg
    (let ((meta (get-text-property 0 'meta arg)))
      (when (string-match (concat (regexp-quote arg) " *(") meta)
        (with-temp-buffer
          (let ((start (match-end 0)))
            (insert meta)
            (goto-char start)
            (condition-case nil
                (forward-sexp)
              (scan-error
               (goto-char (point-max))))
            (buffer-substring-no-properties
             start (point))))))))

;;;###autoload
(defun cxrefs-capf ()
  "Provide capf interface for `completion-at-point-functions'."
  (let ((bounds (find-tag-default-bounds)))
    (when (and bounds (cxrefs-capf--get-ctx))
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic
             (lambda (prefix)
               (let ((candidates (cxrefs-capf--lookup prefix)))
                 (if (listp candidates)
                     candidates
                   nil))))
            :exclusive 'no
            ;:company-kind (lambda (_) 'function)
            :company-location (lambda (x)
                                (and x (get-text-property 0 'location x)))
            :company-docsig (lambda (x)
                              (and x (get-text-property 0 'meta x)))
            :annotation-function #'cxrefs-capf--annotation))))

(provide 'cxrefs-capf)
;;; cxrefs-capf.el ends here
