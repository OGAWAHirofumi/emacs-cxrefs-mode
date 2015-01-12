;;; cxrefs.el --- Cross reference for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2001-2015  OGAWA Hirofumi

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'ring)
(require 'thingatpt)

(defgroup cxrefs-cscope nil
  "Cxrefs cscope backend."
  :version "25.1"
  :group 'cxrefs-cscope)

(defcustom cxrefs-cscope-program "cscope"
  "The name of the cscope executable."
  :group 'cxrefs-cscope)

(defcustom cxrefs-cscope-build-files-history
  '("cscope-linux.pl"
    "find -L . -name '*.[chlyS]' -o -name '*.cc' -o -name '*.cpp' > cscope.files"
    "cscope-freebsd.pl"
    "cscope-glibc.pl")
  "*Command for creating cscope.file."
  :group 'cxrefs-cscope)

(defvar cxrefs-cscope-command-table
  '((symbol		. "0") (define		. "1")
    (callee		. "2") (caller		. "3")
    (text		. "4") (grep		. "5")
    (egrep		. "6") (file		. "7")
    (includer		. "8") (assign		. "9")
    (all		. "a") (toggle-case	. "c")
    (rebuild		. nil) (quit		. nil)))

(defun cxrefs-cscope-check-db (ctx)
  (file-exists-p (concat (cxrefs-ctx-dir-get ctx) "cscope.out")))

(defun cxrefs-cscope-init (ctx &optional option)
  (let ((process-connection-type nil) ; use a pipe
	(default-directory (cxrefs-ctx-dir-get ctx))
	(program-args '("-l"))
	(buffer (generate-new-buffer "*Cscope-Process*"))
	process)
    (buffer-disable-undo buffer)
    (when (and (not (eq option 'rebuild)) (cxrefs-cscope-check-db ctx))
      (push "-d" program-args))
    (setq process (apply 'start-process "Cscope-Process" buffer
			 cxrefs-cscope-program program-args))
    (set-process-query-on-exit-flag process nil)
    ;; Wait prompt
    (accept-process-output process)
    process))

(defun cxrefs-cscope-ask-files-command (basedir)
  "Retrun a command for creating cscope.files."
  ;; If we don't know the command yet, ask user.
  (let ((cmd (read-string "Command for creating cscope.files: "
			  (car cxrefs-cscope-build-files-history)
			  '(cxrefs-cscope-build-files-history . 1))))
    (when (string= cmd "")
      (error "Invalid command for creating cscope.files"))
    (format cmd basedir)))

(defun cxrefs-cscope-build-db (ctx)
  "Build cscope.files."
  (let* ((basedir (cxrefs-ctx-dir-get ctx))
	 (command (cxrefs-cscope-ask-files-command basedir))
	 (default-directory basedir))
    ;; Build cscope.files
    (unless (= 0 (shell-command command))
      (error "Couldn't create cscope.files"))))

(defun cxrefs-cscope-make-xref (ctx command string)
  (let ((process (cxrefs-ctx-process-get ctx))
	xref)
    (with-current-buffer (process-buffer process)
      (erase-buffer)
      (process-send-string process (concat command string "\n"))
      (accept-process-output process)
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\(.+?\\) \\(\\S-+?\\) \\([0-9]+?\\) \\(.*\\)" nil t)
	(let ((file (match-string 1))
	      (func (match-string 2))
	      (line (match-string 3))
	      (hint (match-string 4)))
	  (push (cxrefs-xref-make ctx func file line hint) xref))))
    (nreverse xref)))

(defun cxrefs-cscope-command (ctx cmd-type &optional string)
  (let ((cmd (cdr (assoc cmd-type cxrefs-cscope-command-table))))
    (cond
     ((stringp cmd)
      (cxrefs-process-init ctx)
      (cxrefs-cscope-make-xref ctx cmd string))
     ((eq cmd-type 'rebuild)
      ;; Quit if alive
      (when (cxrefs-process-check ctx)
	(cxrefs-cscope-make-xref ctx "q" ""))
      ;; Then rebuild
      (cxrefs-process-init ctx 'rebuild)
      (cxrefs-cscope-make-xref ctx "r" ""))
     ((eq cmd-type 'quit)
      (let ((process (cxrefs-ctx-process-get ctx)))
	(when (processp process)
	  (cxrefs-cscope-make-xref ctx "q" "")
	  (when (process-buffer process)
	    (kill-buffer (process-buffer process)))))
      (cxrefs-ctx-process-set ctx nil)))
    ))

(defvar cxrefs-backend-cscope '("cscope"
				:init-fn cxrefs-cscope-init
				:check-db-fn cxrefs-cscope-check-db
				:build-db-fn cxrefs-cscope-build-db
				:command-fn cxrefs-cscope-command))

(defgroup cxrefs-gtags nil
  "Cxrefs gtags backend."
  :version "25.1"
  :group 'cxrefs-gtags)

(defcustom cxrefs-gtags-program "gtags-cscope"
  "The name of the gtags executable."
  :group 'cxrefs-gtags)

(defcustom cxrefs-gtags-build-program "gtags"
  "The name of the gtags executable to make database."
  :group 'cxrefs-gtags)

(defun cxrefs-gtags-check-db (ctx)
  (file-exists-p (concat (cxrefs-ctx-dir-get ctx) "GTAGS")))

(defun cxrefs-gtags-init (ctx &optional _option)
  (let ((process-connection-type nil) ; use a pipe
	(default-directory (cxrefs-ctx-dir-get ctx))
	(buffer (generate-new-buffer "*Gtags-Process*"))
	process)
    (buffer-disable-undo buffer)
    (setq process (start-process "Gtags-Process" buffer cxrefs-gtags-program))
    (set-process-query-on-exit-flag process nil)
    ;; Wait prompt
    (accept-process-output process)
    process))

(defun cxrefs-gtags-build-db (ctx)
  "Build GTAGS."
  (let* ((basedir (cxrefs-ctx-dir-get ctx))
	 (command cxrefs-gtags-build-program)
	 (default-directory basedir))
    ;; Build cscope.files
    (unless (= 0 (shell-command command))
      (error "Couldn't create cscope.files"))))

(defun cxrefs-gtags-command (ctx cmd-type &optional string)
  (if (not (eq cmd-type 'rebuild))
      (cxrefs-cscope-command ctx cmd-type string)
    ;; rebuild
    (cxrefs-process-init ctx 'rebuild)
    (cxrefs-cscope-make-xref ctx "r" "")))

(defvar cxrefs-backend-gtags '("gtags"
			       :init-fn cxrefs-gtags-init
			       :check-db-fn cxrefs-gtags-check-db
			       :build-db-fn cxrefs-gtags-build-db
			       :command-fn cxrefs-gtags-command))

(defvar cxrefs-backends
  (list cxrefs-backend-cscope
	cxrefs-backend-gtags)
  "Cxrefs backend list.")

(defgroup cxrefs nil
  "Support for cross reference."
  :version "25.1"
  :group 'cxrefs)

(defcustom cxrefs-backend-default "cscope"
  "Default backend."
  :group 'cxrefs)

(defcustom cxrefs-hierarchy-depth 4
  "Default depth for function hierarchy."
  :group 'cxrefs)

(defcustom cxrefs-min-location-width 10
  "Minimum width to show filename:line."
  :group 'cxrefs)
(defcustom cxrefs-min-function-width 10
  "Minimum width to show function."
  :group 'cxrefs)

(defcustom cxrefs-marker-length 32
  "Length of marker history."
  :group 'cxrefs)
(defcustom cxrefs-selbuf-length 32
  "Length of select buffer history."
  :group 'cxrefs)

(defun cxrefs-ctx-backend-set (ctx backend)
  (setq ctx (plist-put ctx :backend backend)))
(defun cxrefs-ctx-process-get (ctx)
  (plist-get ctx :process))
(defun cxrefs-ctx-process-set (ctx process)
  (setq ctx (plist-put ctx :process process)))
(defun cxrefs-ctx-dir-get (ctx)
  (plist-get ctx :dir))
(defun cxrefs-ctx-dir-set (ctx dir)
  (setq ctx (plist-put ctx :dir dir)))
(defun cxrefs-ctx-marker-get (ctx)
  (plist-get ctx :marker))
(defun cxrefs-ctx-marker-set (ctx history)
  (setq ctx (plist-put ctx :marker history)))
(defun cxrefs-ctx-selbuf-get (ctx)
  (plist-get ctx :selbuf))
(defun cxrefs-ctx-selbuf-set (ctx history)
  (setq ctx (plist-put ctx :selbuf history)))
(defun cxrefs-ctx-window-config-get (ctx)
  (plist-get ctx :config))
(defun cxrefs-ctx-window-config-set (ctx config)
  (setq ctx (plist-put ctx :config config)))

(defun cxrefs-ctx-backend (ctx)
  (car (plist-get ctx :backend)))
(defun cxrefs-ctx-backend-fn (ctx sym)
  (plist-get (cdr (plist-get ctx :backend)) sym))
(defun cxrefs-backend-init (ctx &optional option)
  (funcall (cxrefs-ctx-backend-fn ctx :init-fn) ctx option))
(defun cxrefs-backend-check-db (ctx)
  (funcall (cxrefs-ctx-backend-fn ctx :check-db-fn) ctx))
(defun cxrefs-backend-build-db (ctx)
  (funcall (cxrefs-ctx-backend-fn ctx :build-db-fn) ctx))
(defun cxrefs-backend-command (ctx cmd-type &optional string)
  (funcall (cxrefs-ctx-backend-fn ctx :command-fn) ctx cmd-type string))

;; Helper for backend
(defun cxrefs-process-check (ctx)
  (let ((process (cxrefs-ctx-process-get ctx)))
    (and (processp process) (process-live-p process))))

(defun cxrefs-process-init (ctx &optional option)
  (unless (cxrefs-process-check ctx)
    (cxrefs-ctx-process-set ctx (cxrefs-backend-init ctx option))))

(defun cxrefs-xref-make (ctx func file line hint)
  (let* ((file (cxrefs-relative-file-name ctx file))
	 (linenum (if (stringp line) (string-to-number line) line))
	 (location (format "%s:%d" file linenum))
	 (loc-len (length location))
	 (func-len (length func)))
    (list :func-len func-len :func func
	  :loc-len loc-len :location location
	  :file file :line linenum
	  :hint hint)))

;; Utility functions
(defun cxrefs-relative-file-name (ctx file)
  (let* ((basedir (cxrefs-ctx-dir-get ctx))
	 (absolute (expand-file-name file basedir)))
    (if (string-prefix-p basedir absolute)
	(file-relative-name absolute basedir)
      (identity absolute))))

(defun cxrefs-expand-file-name (ctx file)
  (let ((basedir (cxrefs-ctx-dir-get ctx)))
    (if (file-name-absolute-p file)
	(identity file)
      (concat basedir file))))

(defun cxrefs-xref-output (buffer ctx cmd-type string xref)
  (let ((func-max (apply 'max cxrefs-min-function-width
			 (mapcar (lambda (x) (plist-get x :func-len)) xref)))
	(loc-max (apply 'max cxrefs-min-location-width
			(mapcar (lambda (x) (plist-get x :loc-len)) xref))))
    (with-current-buffer buffer
      (let ((loc-width (number-to-string loc-max))
	    (func-width (number-to-string func-max)))
	;; Insert headers
	(insert "-*- mode: cxrefs-select -*-\n")
	(insert (format "Dir: %s\n" (cxrefs-ctx-dir-get ctx)))
	(insert (format "Backend: %s\n" (cxrefs-ctx-backend ctx)))
	(insert (format "Find[%s]: %s\n" cmd-type string))
	(insert "\n")
	(dolist (x xref)
	  ;; Insert xrefs
	  (insert (format (concat "%-" func-width "s %-" loc-width "s %s\n")
			  (plist-get x :func) (plist-get x :location)
			  (plist-get x :hint))))))
    ))

;; History management helpers
(defun cxrefs-history-make (hlen &optional kill-fn)
  (list :pos 0 :kill-fn kill-fn :ring (make-ring hlen)))
(defun cxrefs-history-kill-fn-get (history)
  (plist-get history :kill-fn))
(defun cxrefs-history-pos-get (history)
  (plist-get history :pos))
(defun cxrefs-history-pos-set (history pos)
  (setq history (plist-put history :pos pos)))
(defun cxrefs-history-ring-get (history)
  (plist-get history :ring))

(defun cxrefs-history-pos-top-p (history)
  (= 0 (cxrefs-history-pos-get history)))

(defun cxrefs-history-reset (history)
  "Remove all history."
  ;; Clear out the history we are throwing away.
  (let ((kill-fn (cxrefs-history-kill-fn-get history))
	(ring (cxrefs-history-ring-get history)))
    (while (not (ring-empty-p ring))
      (let ((item (ring-remove ring 0)))
	(when item
	  (funcall kill-fn item)))))
  (cxrefs-history-pos-set history 0))

(defun cxrefs-history-pop (history)
  "Remove newest history."
  (let ((ring (cxrefs-history-ring-get history)))
    (cxrefs-history-pos-set history 0)
    (or (ring-empty-p ring) (ring-remove ring 0))))

(defun cxrefs-history-push (history new)
  "Add new history as newest history."
  (let* ((ring (cxrefs-history-ring-get history))
	 (kill-fn (cxrefs-history-kill-fn-get history))
	 (full (>= (ring-length ring) (ring-size ring)))
	 (old (and full (ring-remove ring nil))))
    (when (and old kill-fn)
      (funcall kill-fn old))
    (cxrefs-history-pos-set history 0)
    (ring-insert ring new)))

(defun cxrefs-history-add (history new)
  "Add new history and remove all forward histories."
  (let ((ring (cxrefs-history-ring-get history))
	(kill-fn (cxrefs-history-kill-fn-get history))
	(pos (cxrefs-history-pos-get history)))
    (when kill-fn
      (while (> pos 0)
	(let ((old (ring-remove ring 0)))
	  (funcall kill-fn old))
	(setq pos (1- pos))))
    (cxrefs-history-push history new)))

(defun cxrefs-history-update-lru (history)
  "Remove current history and re-add it as newest history."
  (let ((ring (cxrefs-history-ring-get history))
	(pos (cxrefs-history-pos-get history)))
    (cxrefs-history-pos-set history 0)
    (when (not (ring-empty-p ring))
      (ring-insert ring (ring-remove ring pos)))))

(defun cxrefs-history-go-prev (history)
  "Go previous history and return it."
  (let ((ring (cxrefs-history-ring-get history))
	(prev-pos (1+ (cxrefs-history-pos-get history))))
    (if (> prev-pos (1- (ring-length ring)))
	nil
      (cxrefs-history-pos-set history prev-pos)
      (ring-ref ring prev-pos))))

(defun cxrefs-history-go-next (history)
  "Go next history and return it."
  (let ((ring (cxrefs-history-ring-get history))
	(next-pos (1- (cxrefs-history-pos-get history))))
    (if (< next-pos 0)
	nil
      (cxrefs-history-pos-set history next-pos)
      (ring-ref ring next-pos))))

(defun cxrefs-history-current (history)
  "Retrun current history."
  (let ((ring (cxrefs-history-ring-get history)))
    (if (ring-empty-p ring)
	nil
      (ring-ref ring (cxrefs-history-pos-get history)))))

;;; Marker history functions
(defun cxrefs-marker-kill-fn (marker)
  (set-marker marker nil))

(defun cxrefs-marker-make ()
  (cxrefs-history-make cxrefs-marker-length 'cxrefs-marker-kill-fn))

(defun cxrefs-marker-reset (history)
  ;; Clear out the markers we are throwing away.
  (when history
    (cxrefs-history-reset history)))

(defun cxrefs-marker-push (history)
  ;; if this is top, remove dummy entry
  (when (cxrefs-history-pos-top-p history)
    (cxrefs-history-pop history))
  ;; save current point
  (cxrefs-history-add history (point-marker))
  ;; and add dummy marker
  (cxrefs-history-add history nil))

;; Select buffer history functions
(defun cxrefs-selbuf-make ()
  (cxrefs-history-make cxrefs-selbuf-length 'kill-buffer))

(defun cxrefs-selbuf-reset (history)
  ;; Clear out the markers we are throwing away.
  (if history
      (cxrefs-history-reset history)))

(defun cxrefs-selbuf-push (history buffer)
  (cxrefs-history-push history buffer))

(defun cxrefs-selbuf-update-lru (history)
  (cxrefs-history-update-lru history))

(defun cxrefs-selbuf-current (history)
  (cxrefs-history-current history))

;; Cxrefs context management
(defvar cxrefs-context-hash (make-hash-table :test 'equal :weakness nil))
(defvar cxrefs-basedir nil)
(make-variable-buffer-local 'cxrefs-basedir)

(defun cxrefs-context-find (basedir)
  (gethash basedir cxrefs-context-hash))
(defun cxrefs-context-add (ctx)
  (puthash (cxrefs-ctx-dir-get ctx) ctx cxrefs-context-hash))
(defun cxrefs-context-del (ctx)
  (remhash (cxrefs-ctx-dir-get ctx) cxrefs-context-hash))

(defun cxrefs-basedir-list ()
  "Return the all cxrefs-basedir."
  (maphash (lambda (_key ctx) (cxrefs-ctx-dir-get ctx)) cxrefs-context-hash))

(defun cxrefs-context-current ()
  "Retrun the current cxrefs context."
  (cxrefs-context-find cxrefs-basedir))

(defun cxrefs-context-make (basedir)
  "Initialize the cxrefs context with BASEDIR."
  (let ((ctx (cxrefs-ctx-dir-set () basedir)))
    (cxrefs-ctx-marker-set ctx (cxrefs-marker-make))
    (cxrefs-ctx-selbuf-set ctx (cxrefs-selbuf-make))
    (cxrefs-context-add ctx)))

(defun cxrefs-context-delete (ctx)
  "Delete the cxrefs context associated with BASEDIR."
  (when ctx
    (cxrefs-marker-reset (cxrefs-ctx-marker-get ctx))
    (cxrefs-selbuf-reset (cxrefs-ctx-selbuf-get ctx))
    (cxrefs-context-del ctx)))

(defvar cxrefs-no-context-error "No associated context for cxrefs")

;; Marker history interface
(defun cxrefs-marker-go-next ()
  "Go next to where point was newer invoked."
  (interactive)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(error cxrefs-no-context-error)
      (let ((marker (cxrefs-history-go-next (cxrefs-ctx-marker-get ctx))))
	(unless marker
	  (error "No forward locations for cxrefs"))
	(switch-to-buffer (or (marker-buffer marker)
			      (error "The marked buffer has been deleted")))
	(goto-char (marker-position marker))))))

(defun cxrefs-marker-go-prev ()
  "Go previous to where point was last invoked."
  (interactive)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(error cxrefs-no-context-error)
      (let ((history (cxrefs-ctx-marker-get ctx)))
	;; if this is top, replace it with current point
	(when (cxrefs-history-pos-top-p history)
	  (cxrefs-history-pop history)
	  (cxrefs-history-push history (point-marker)))
	;; go back previous marker
	(let ((marker (cxrefs-history-go-prev history)))
	  (unless marker
	    (error "No previous locations for cxrefs"))
	  (switch-to-buffer (or (marker-buffer marker)
				(error "The marked buffer has been deleted")))
	  (goto-char (marker-position marker)))))))

;; Select buffer history interface
(defun cxrefs-selbuf-go-next ()
  "Go next select buffer."
  (interactive)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(error cxrefs-no-context-error)
      (let ((next (cxrefs-history-go-next (cxrefs-ctx-selbuf-get ctx))))
	(if (not (bufferp next))
	    (error "No next select buffer"))
	(switch-to-buffer next)))))

(defun cxrefs-selbuf-go-prev ()
  "Go previous select buffer."
  (interactive)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(error cxrefs-no-context-error)
      (let ((prev (cxrefs-history-go-prev (cxrefs-ctx-selbuf-get ctx))))
	(if (not (bufferp prev))
	    (error "No previous select buffer"))
	(switch-to-buffer prev)))))

;; Hooks
(defvar cxrefs-run-command-hook nil)
(defvar cxrefs-back-and-next-select-hook nil)
(defvar cxrefs-file-not-found-hook nil)
(defvar cxrefs-select-interpret-line-hook nil)
(defvar cxrefs-select-mode-hook nil)

(defun cxrefs-ask-basedir ()
  "Tell tags commands to use tag table file FILE."
  (let* ((file-name-history (cxrefs-basedir-list))
	 (basedir (read-directory-name "Locate root directory: "
				       default-directory default-directory t)))
    ;; Make sure the last char of "root" is `/'
    (file-name-as-directory (expand-file-name basedir))))

;; Find exists databases for each backends. If no database, use default.
(defun cxrefs-select-backend (ctx)
  (let (result)
    (dolist (backend cxrefs-backends result)
      (unless result
	(cxrefs-ctx-backend-set ctx backend)
	(when (cxrefs-backend-check-db ctx)
	  (setq result backend))))
    (unless result
      (setq result (assoc cxrefs-backend-default cxrefs-backends)))
    (message "Use %s backend." (car result))
    result))

(defun cxrefs-check-and-build-db ()
  (let ((ctx (cxrefs-context-current)))
    (unless (cxrefs-ctx-backend ctx)
      (cxrefs-ctx-backend-set ctx (cxrefs-select-backend ctx)))
    (unless (cxrefs-backend-check-db ctx)
      (cxrefs-backend-build-db ctx))))

(defun cxrefs-check-tags-table ()
  (unless cxrefs-basedir
    (setq cxrefs-basedir (cxrefs-ask-basedir)))
  (unless (cxrefs-context-current)
    (cxrefs-context-make cxrefs-basedir))
  (cxrefs-check-and-build-db))

;; Make hierarchy by reverse order
(defun cxrefs-xref-hierarchy2 (ctx cmd-type func whole depth max-depth
				   arrow exclude)
  (let ((prefix (format "%s%s" (make-string depth ? ) arrow))
	(xref (cxrefs-backend-command ctx cmd-type func)))
    (dolist (x xref whole)
      (let ((next-func (plist-get x :func))
	    (file (plist-get x :file)))
	(when (or (string= exclude "") (not (string-match exclude file)))
	  ;; Add hierarchy annotation to function
	  (let ((prefix-func (format "%s %s" prefix next-func)))
	    (setq x (plist-put x :func prefix-func))
	    (setq x (plist-put x :func-len (length prefix-func))))
	  ;; Make whole list by reverse order
	  (push (plist-put x :depth depth) whole)
	  (when (< depth max-depth)
	    (setq whole (cxrefs-xref-hierarchy2 ctx cmd-type next-func whole
						(1+ depth) max-depth
						arrow exclude)))))
      )))
(defun cxrefs-xref-hierarchy (ctx cmd-type string args)
  (let* ((max-depth (nth 0 args))
	 (exclude (nth 1 args))
	 (cmd-type-table '((caller-hierarchy caller "<-")
			   (callee-hierarchy callee "->")))
	 (type (nth 1 (assoc cmd-type cmd-type-table)))
	 (arrow (nth 2 (assoc cmd-type cmd-type-table))))
    (nreverse (cxrefs-xref-hierarchy2 ctx type string nil 0 max-depth
				      arrow exclude))))

(defun cxrefs-xref-command (ctx cmd-type string args)
  (if (or (eq cmd-type 'caller-hierarchy) (eq cmd-type 'callee-hierarchy))
      (cxrefs-xref-hierarchy ctx cmd-type string args)
    (cxrefs-backend-command ctx cmd-type string)))

(defun cxrefs-run-command (cmd-type string &rest args)
  (cxrefs-check-tags-table)
  (let* ((ctx (cxrefs-context-current))
	 (select-buffer-name (format "*Cxrefs (%s)*" string))
	 (old-buffer (cxrefs-selbuf-current (cxrefs-ctx-selbuf-get ctx)))
	 (buffer (generate-new-buffer select-buffer-name)))
    (buffer-disable-undo buffer)
    (cxrefs-ctx-window-config-set ctx (current-window-configuration))
    (cxrefs-marker-push (cxrefs-ctx-marker-get ctx))
    (cxrefs-selbuf-push (cxrefs-ctx-selbuf-get ctx) buffer)
    (if (and (bufferp old-buffer) (get-buffer-window old-buffer))
	;; if there is select-buffer, use it
	(progn
	  (select-window (get-buffer-window old-buffer))
	  (switch-to-buffer buffer))
      (pop-to-buffer buffer))
    ;; Insert cxrefs-select output
    (let ((xref (cxrefs-xref-command ctx cmd-type string args)))
      (cxrefs-xref-output buffer ctx cmd-type string xref))
    (goto-char (point-min))
    (cxrefs-select-mode)
    (run-hooks 'cxrefs-run-command-hook)))

(defun cxrefs-read-string (prompt type)
  (read-string prompt (or (thing-at-point type) "")))

(defun cxrefs-find-definition (string)
  "Query the definition of the given STRING."
  (interactive (list (cxrefs-read-string "Find this definition: " 'symbol)))
  (cxrefs-run-command 'define string))

(defun cxrefs-find-symbol (string)
  "Find this symbol STRING."
  (interactive (list (cxrefs-read-string "Find this symbol: " 'symbol)))
  (cxrefs-run-command 'symbol string))

(defun cxrefs-find-callee (string)
  "Find callee of symbol STRING."
  (interactive (list (cxrefs-read-string "Find callee of this: " 'symbol)))
  (cxrefs-run-command 'callee string))

(defun cxrefs-find-caller (string)
  "Find caller of symbol STRING."
  (interactive (list (cxrefs-read-string "Find caller of this: " 'symbol)))
  (cxrefs-run-command 'caller string))

(defun cxrefs-find-text (string)
  "Find this text STRING."
  (interactive (list (cxrefs-read-string "Find this text: " 'text)))
  (cxrefs-run-command 'text string))

(defun cxrefs-find-grep (string)
  "Find this grep pattern STRING."
  (interactive (list (cxrefs-read-string "Find this grep pattern: " 'text)))
  (cxrefs-run-command 'grep string))

(defun cxrefs-find-egrep (string)
  "Find this egrep pattern STRING."
  (interactive (list (cxrefs-read-string "Find this egrep pattern: " 'text)))
  (cxrefs-run-command 'egrep string))

(defun cxrefs-find-file (string)
  "Find this file STRING."
  (interactive (list (cxrefs-read-string "Find this file: " 'filename)))
  (cxrefs-run-command 'file string))

(defun cxrefs-find-includer (string)
  "Find files #including this file STRING."
  (interactive (list
		(cxrefs-read-string "Find files #including this: " 'filename)))
  (cxrefs-run-command 'includer string))

(defun cxrefs-find-assign (string)
  "Find assignments to this symbol STRING."
  (interactive (list (cxrefs-read-string "Find assignments to this: " 'symbol)))
  (cxrefs-run-command 'assign string))

(defun cxrefs-toggle-case ()
  "Toggle ignore/use letter case."
  (interactive)
  (let ((ctx (cxrefs-context-current)))
    (when ctx
      (cxrefs-backend-command ctx 'toggle-case)
      (message "Toggle case cxrefs"))))

(defun cxrefs-rebuild ()
  "Rebuild cross reference database."
  (interactive)
  (let ((ctx (cxrefs-context-current)))
    (when ctx
      (cxrefs-backend-command ctx 'rebuild)))
  (message "Rebuild cxrefs"))

(defun cxrefs-quit ()
  "Quit backend and context."
  (interactive)
  (let ((ctx (cxrefs-context-current)))
    (when ctx
      (cxrefs-backend-command ctx 'quit)
      (cxrefs-context-delete ctx)))
  (setq cxrefs-basedir nil)
  (message "Quit cxrefs"))

(defun cxrefs-find-caller-hierarchy (string depth exclude)
  "Find hierarchical callers of STRING."
  (interactive (list (cxrefs-read-string "Find hierarchical callers: " 'symbol)
		     (read-number "Depth: " cxrefs-hierarchy-depth)
		     (read-string "Exclude files matching this regexp: ")))
  (cxrefs-run-command 'caller-hierarchy string depth exclude))

(defun cxrefs-find-callee-hierarchy (string depth exclude)
  "Find hierarchical callee of STRING."
  (interactive (list (cxrefs-read-string "Find hierarchical callees: " 'symbol)
		     (read-number "Depth: " cxrefs-hierarchy-depth)
		     (read-string "Exclude files matching this regexp: ")))
  (cxrefs-run-command 'callee-hierarchy string depth exclude))

(defun cxrefs-back-and-next-select ()
  "Return to the cxrefs-select buffer and advance the cursor by one line."
  (interactive)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(error "Cxrefs no associated context")
      (let ((buffer (cxrefs-selbuf-current (cxrefs-ctx-selbuf-get ctx))))
	(if (not (bufferp buffer))
	    (error "Cxrefs no select buffer")
	  (cxrefs-ctx-window-config-set ctx (current-window-configuration))
	  (pop-to-buffer buffer t)))
      (cxrefs-select-next-line)
      (run-hooks 'cxrefs-back-and-next-select-hook))))

(defvar cxrefs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-N"	'cxrefs-marker-go-next)
    (define-key map "\M-P"	'cxrefs-marker-go-prev)
    (define-key map "\M-*"	'cxrefs-marker-go-prev)
    (define-key map "\M-."	'cxrefs-find-definition)
    (define-key map "\C-cc"	'cxrefs-find-symbol)
    (define-key map "\C-cv"	'cxrefs-find-callee)
    (define-key map "\C-c^"	'cxrefs-find-caller)
    (define-key map "\C-ct"	'cxrefs-find-text)
    (define-key map "\C-cg"	'cxrefs-find-grep)
    (define-key map "\C-ce"	'cxrefs-find-egrep)
    (define-key map "\C-cf"	'cxrefs-find-file)
    (define-key map "\C-ci"	'cxrefs-find-includer)
    (define-key map "\C-c="	'cxrefs-find-assign)
    (define-key map "\C-ch"	'cxrefs-find-caller-hierarchy)
    (define-key map "\C-cH"	'cxrefs-find-callee-hierarchy)
;    (define-key map "\C-ca"	'cxrefs-toggle-case)
;    (define-key map "\C-cr"	'cxrefs-rebuild)
    (define-key map "\C-cq"	'cxrefs-quit)
    (define-key map "\C-c\C-c"	'cxrefs-back-and-next-select)
    map)
  "Keymap used in Cxrefs mode.")

;;;###autoload
(define-minor-mode cxrefs-mode
  "Toggle Cxrefs minor mode.
With prefix ARG, turn Auto-insert mode on if and only if ARG is positive.
Returns the new status of Auto-insert mode (non-nil means on).

Turning on Cxrefs mode calls the value of the variable `cxrefs-mode-hook'
with no args, if that value is non-nil.

\\{cxrefs-mode-map}"
  :group 'cxrefs
  :version "25.1"
  :lighter " Cxrefs"
  :keymap cxrefs-mode-map)
;  (when cxrefs-mode (cxrefs-check-tags-table)))

;; cxrefs-select output parser
(defvar cxrefs-select-output-basedir
  "^Dir: \\(.*\\)$")
(defvar cxrefs-select-output-backend
  "^Backend: \\(.*\\)$")
(defvar cxrefs-output-line-regexp
  "^\\s-*\\(->\\|<-\\)?\\s-*\\(.+?\\)\\s-+\\(\\S-+\\):\\([0-9]+\\)\\s-+\\(.*\\)"
  "This regular expression is used to recognize valid reference lines.")
(defvar cxrefs-output-func-place 2
  "Position number of `cxrefs-output-line-regexp' which locates function name.")
(defvar cxrefs-output-file-place 3
  "Position number of `cxrefs-output-line-regexp' which locates filename.")
(defvar cxrefs-output-line-place 4
  "Position number of `cxrefs-output-line-regexp' which locates line number.")

(defun cxrefs-select-next-line (&optional count)
  "Cxrefs select mode next line."
  (interactive)
  (end-of-line)
  (re-search-forward cxrefs-output-line-regexp nil t count)
  (beginning-of-line))

(defun cxrefs-select-previous-line (&optional count)
  "Cxrefs select mode previous line."
  (interactive)
  (beginning-of-line)
  (re-search-backward cxrefs-output-line-regexp nil t count)
  (beginning-of-line))

(defun cxrefs-file-not-found ()
  (if cxrefs-file-not-found-hook
      (run-hooks 'cxrefs-file-not-found-hook)
    (kill-buffer (get-file-buffer (buffer-file-name)))
    (error "Cxrefs can't find file %s" (buffer-file-name))))

(defun cxrefs-select-interpret-line (&optional preview)
  "Parse the line under the cursor as a cxrefs output reference line."
  (interactive "P")
  (beginning-of-line)
  (if (not (looking-at cxrefs-output-line-regexp))
      (error "Cxrefs Line not understood as a cxrefs reference line")
    (let ((select-mode-window (get-buffer-window (current-buffer)))
	  (ctx (cxrefs-context-current))
	  (func (match-string cxrefs-output-func-place))
	  (file (match-string cxrefs-output-file-place))
	  (line (match-string cxrefs-output-line-place))
	  (find-file-not-found-functions (list 'cxrefs-file-not-found)))
      (find-file-other-window (cxrefs-expand-file-name ctx file))
      ;; Inherit cxrefs-basedir
      (setq cxrefs-basedir (cxrefs-ctx-dir-get ctx))
      (let ((history (cxrefs-ctx-selbuf-get ctx)))
	(when history
	  (cxrefs-selbuf-update-lru history)))
      (cxrefs-mode 1)
      ;; Change point to target line
      (goto-char (point-min))
      (forward-line (1- (string-to-number line)))
      (message "Cxrefs Function: %s" func)
      (if preview
	  (select-window select-mode-window)
	(delete-other-windows))
      (run-hooks 'cxrefs-select-interpret-line-hook))))

(defun cxrefs-select-preview ()
  "Call `cxrefs-select-interpret-line' without selecting other window."
  (interactive)
  (cxrefs-select-interpret-line t))

(defun cxrefs-select-quit ()
  "Quit select-mode, then restore window configuration."
  (interactive)
  (let ((ctx (cxrefs-context-current)))
    (when ctx
      (let ((config (cxrefs-ctx-window-config-get ctx)))
	(when (window-configuration-p config)
	  (set-window-configuration config))))))

(defun cxrefs-select-read-context ()
  (goto-char (point-min))
  ;; Set basdir
  (re-search-forward cxrefs-select-output-basedir nil nil)
  (setq cxrefs-basedir (match-string 1))
  ;; Set backend if need
  (re-search-forward cxrefs-select-output-backend nil nil)
  (let ((backend (match-string 1)))
    (when (not (cxrefs-context-current))
      (cxrefs-context-make cxrefs-basedir))
    (let ((ctx (cxrefs-context-current)))
      (when (not (cxrefs-ctx-backend ctx))
	(cxrefs-ctx-backend-set ctx (assoc backend cxrefs-backends))))
    ))

(defvar cxrefs-select-font-lock-keywords
  `((,cxrefs-select-output-basedir (1 'font-lock-keyword-face))
    (,cxrefs-select-output-backend (1 'font-lock-string-face))
    ("^Find\\[.+?\\]: \\(.*\\)$" (1 'font-lock-string-face))
    (,cxrefs-output-line-regexp
     (,cxrefs-output-func-place 'font-lock-function-name-face)
     (,cxrefs-output-file-place 'font-lock-keyword-face)
     (,cxrefs-output-line-place 'font-lock-keyword-face))))

(defconst cxrefs-select-font-lock-defaults
  '(cxrefs-select-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))

(defvar cxrefs-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cxrefs-mode-map)
    (define-key map "\M-n"	'cxrefs-selbuf-go-next)
    (define-key map "\M-p"	'cxrefs-selbuf-go-prev)
    (define-key map "n"		'cxrefs-select-next-line)
    (define-key map "p"		'cxrefs-select-previous-line)
    (define-key map "."		'cxrefs-find-definition)
    (define-key map "c"		'cxrefs-find-symbol)
    (define-key map "v"		'cxrefs-find-callee)
    (define-key map "^"		'cxrefs-find-caller)
    (define-key map "t"		'cxrefs-find-text)
    (define-key map "g"		'cxrefs-find-grep)
    (define-key map "e"		'cxrefs-find-egrep)
    (define-key map "f"		'cxrefs-find-file)
    (define-key map "i"		'cxrefs-find-includer)
    (define-key map "="		'cxrefs-find-assign)
    (define-key map "h"		'cxrefs-find-caller-hierarchy)
    (define-key map "H"		'cxrefs-find-callee-hierarchy)
    (define-key map "C"		'cxrefs-toggle-case)
    (define-key map "R"		'cxrefs-rebuild)
    (define-key map "\C-c\C-c"	'cxrefs-select-interpret-line)
    (define-key map "\C-m"	'cxrefs-select-interpret-line)
    (define-key map [return]	'cxrefs-select-interpret-line)
    (define-key map " "		'cxrefs-select-preview)
    (define-key map "q"		'cxrefs-select-quit)
    map)
  "Keymap used in Cxrefs select mode.")

;;;###autoload
(defun cxrefs-select-mode ()
  "Major mode for choosing a tag from tags list.
Turning on Cxrefs-Select mode calls the value of the variable
`cxrefs-select-mode-hook' with no args, if that value is non-nil.

\\{cxrefs-select-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'cxrefs-select-mode)
  (setq mode-name "Cxrefs-Select")
  (use-local-map cxrefs-select-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       cxrefs-select-font-lock-defaults)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (cxrefs-select-read-context)
  (goto-char (point-min))
  (cxrefs-select-next-line)
  (run-hooks 'cxrefs-select-mode-hook))

(provide 'cxrefs)
;;; cxrefs.el ends here
