;;; cxrefs-fnmatch.el --- Convert pattern to regexp  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  OGAWA Hirofumi

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Keywords: files, matching

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Example usage:
;;
;; (let ((regexp (cxrefs-wildcard-to-regexp "fs/**/[a-z]??*.c")))
;;   (string-match regexp str))
;; 
;; (let ((regexp (cxrefs-pattern-to-regexp "{fs,mm}/*.c")))
;;   (string-match regexp str))

;;; Code:

(defun cxrefs--wildcard-to-regexp (wildcard)
  ;; wildcard => regexp
  (let ((len (length wildcard))
	(i 0)
	result)
    (while (< i len)
      ;; Copy non-special characters.
      (let ((j (string-match "[][.*+\\^$?]" wildcard i)))
	(when (not j)
	  (setq j len))
	(setq result (concat result (substring wildcard i j)))
	(setq i j))
      ;; Special characters.
      (when (< i len)
	(let ((ch (aref wildcard i)))
	  (setq i (1+ i))
	  (cond
	   ((char-equal ch ?\[)
	    (cond
	     ;; [...
	     ((or (>= i len)
		  (not (string-match "]" wildcard i)))
	      (setq result (concat result "\\[")))
	     ;; []
	     ((and (< i len)
		   (char-equal (aref wildcard i) ?\])
		   ;; not []...]?
		   (or (>= (1+ i) len)
		       (not (string-match "]" wildcard (1+ i)))))
	      (setq result (concat result "\\[\\]"))
	      (setq i (1+ i)))
	     ;; [...]
	     (t
	      ;; [!...] => [^...]
	      (setq result (concat result "["))
	      (when (char-equal (aref wildcard i) ?!)
		(setq result (concat result "^"))
		(setq i (1+ i)))
	      (let ((j (1+ (string-match "]" wildcard i))))
		(setq result (concat result (substring wildcard i j)))
		(setq i j))
	      )))
	   ((char-equal ch ?\])
	    (setq result (concat result "\\]")))
	   ((char-equal ch ?.)
	    (setq result (concat result "\\.")))
	   ((char-equal ch ?+)
	    (setq result (concat result "\\+")))
	   ((char-equal ch ?\\)
	    (setq result (concat result "\\\\")))
	   ((char-equal ch ?^)
	    (setq result (concat result "\\^")))
	   ((char-equal ch ?$)
	    (setq result (concat result "\\$")))
	   ((char-equal ch ??)
	    (setq result (concat result "[^\000/]")))
	   ((char-equal ch ?*)
	    (if (not (string-prefix-p "*" (substring wildcard i)))
		(setq result (concat result "[^\000/]*"))
	      ;; ** => .*
	      (setq result (concat result ".*"))
	      (setq i (1+ i))))
	   ))))
    result))

(defun cxrefs-wildcard-to-regexp (wildcard)
  "Given a shell file name pattern WILDCARD, return an equivalent regexp.
The generated regexp will match a filename only if the filename
matches that wildcard according to shell rules."
  (save-match-data
    (if (null wildcard)
	nil
      (concat "\\`" (cxrefs--wildcard-to-regexp wildcard) "\\'"))))

(defun cxrefs-pattern-to-regexp (wildcard)
  "Similar with `cxrefs-wildcard-to-regexp'.
But this supports brace expansion in WILDCARD."
  (save-match-data
    ;; Brace expansion
    ;; TODO: nested brace is not implemented
    ;; {foo,bar} => \(foo\|bar\)
    (if (null wildcard)
	nil
      (let ((i 0)
	    (result nil))
	(while (string-match "{\\([^},]+\\(,[^},]+\\)+\\)}" wildcard i)
	  (let ((pre (substring wildcard i (match-beginning 0))))
	    (setq i (match-end 0))
	    ;; {..,..,..} => \(..\|..\|..\)
	    (setq result
		  (concat (cxrefs--wildcard-to-regexp pre)
			  "\\("
			  (let ((str-in-brace (match-string 1 wildcard)))
			    (mapconcat 'cxrefs--wildcard-to-regexp
				       (split-string str-in-brace ",")
				       "\\|"))
			  "\\)"))))
	(setq result
	      (concat result
		      (cxrefs--wildcard-to-regexp (substring wildcard i))))
	(concat "\\`" result "\\'")))))

(provide 'cxrefs-fnmatch)
;;; cxrefs-fnmatch.el ends here
