;;; cxrefs.el --- Cross reference for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2001-2018  OGAWA Hirofumi

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
(require 'cxrefs-fnmatch)

(eval-when-compile
  (require 'cl-lib))

(defgroup cxrefs nil
  "Support for cross reference."
  :version "25.1"
  :group 'cxrefs)

(defcustom cxrefs-backend-default 'cscope
  "Default backend."
  :type 'symbol)

(defcustom cxrefs-post-jump-hook '(recenter
				   cxrefs-highlight-position)
  "Hook after jumping to target position.
This hook is called with current window/buffer/position on target."
  :type 'hook)

(defcustom cxrefs-post-select-hook nil
  "Hook after changing a current selected line."
  :type 'hook)

(defcustom cxrefs-hierarchy-depth 4
  "Default depth for function hierarchy."
  :type 'integer)

(defcustom cxrefs-use-filter-regexp nil
  "If non-nil, use filter string as regexp.
If nil, use filter string as shell pattern matching."
  :type 'boolean)

(defcustom cxrefs-fuzzy-target-lines 100
  "If non-nil, search target line from tag hint for this lines range.
With this, even if slightly out of dated tag works."
  :type 'integer)

(defcustom cxrefs-kill-buffers-on-quit 'ask
  "Kill opened buffers (and not `buffer-modified-p') by cxrefs on quit.
If nil, don't kill buffers.  If \\='ask, ask whether kill
buffers or not.  If other, kill buffers without asking."
  :type '(choice (const :tag "ask to user" ask)
		 (const :tag "don't kill buffer" nil)
		 (other :tag "without asking" t)))

(defcustom cxrefs-min-location-width 10
  "Minimum width to show filename:line."
  :type 'integer)
(defcustom cxrefs-min-function-width 10
  "Minimum width to show function."
  :type 'integer)

(defcustom cxrefs-marker-length 32
  "Length of marker history."
  :type 'integer)
(defcustom cxrefs-selbuf-length 32
  "Length of select buffer history."
  :type 'integer)

(defcustom cxrefs-show-excluded-lines nil
  "If non-nil, show excluded lines by filter.  If nil, hidden."
  :type 'boolean)

(defvar cxrefs-excluded-line-sep "Excluded lines..."
  "Separator line for matched and excluded match lines.")

;; FIXME: make this per context
(defvar cxrefs-string-history nil
  "Default input string history list.")

;; FIXME: make this per context
(defvar cxrefs-exclude-history nil
  "Default exclude regexp/pattern history list.")

;; FIXME: make this per context
(defvar cxrefs-include-history nil
  "Default include regexp/pattern history list.")

;; History management helpers
(cl-defstruct (cxrefs-history
	       (:constructor nil)	; no default
	       (:copier nil)
	       (:predicate nil)
	       (:constructor cxrefs-history-make (length
						  kill-fn
						  &aux
						  (ring (make-ring length)))))
  length
  (pos 0)
  (kill-fn nil :read-only t)
  (ring nil :read-only t))

(defun cxrefs-history-pos-top-p (history)
  (= 0 (cxrefs-history-pos history)))

(defun cxrefs-history-reset (history)
  "Remove all history in HISTORY."
  ;; Clear out the history we are throwing away.
  (let ((kill-fn (cxrefs-history-kill-fn history))
	(ring (cxrefs-history-ring history)))
    (while (not (ring-empty-p ring))
      (let ((item (ring-remove ring 0)))
	(when item
	  (funcall kill-fn item)))))
  (setf (cxrefs-history-pos history) 0))

(defun cxrefs-history-pop (history)
  "Remove newest history in HISTORY."
  (let ((ring (cxrefs-history-ring history)))
    (setf (cxrefs-history-pos history) 0)
    (or (ring-empty-p ring) (ring-remove ring 0))))

(defun cxrefs-history-push (history new)
  "Insert onto HISTORY the new history NEW as newest history."
  (let* ((ring (cxrefs-history-ring history))
	 (kill-fn (cxrefs-history-kill-fn history))
	 (full (>= (ring-length ring) (ring-size ring)))
	 (old (and full (ring-remove ring nil))))
    (when (and old kill-fn)
      (funcall kill-fn old))
    (setf (cxrefs-history-pos history) 0)
    (ring-insert ring new)))

(defun cxrefs-history-add (history new)
  "Insert onto HISTORY the new history NEW, then remove all forward histories."
  (let ((ring (cxrefs-history-ring history))
	(kill-fn (cxrefs-history-kill-fn history))
	(pos (cxrefs-history-pos history)))
    (when kill-fn
      (while (> pos 0)
	(let ((old (ring-remove ring 0)))
	  (funcall kill-fn old))
	(setq pos (1- pos))))
    (cxrefs-history-push history new)))

(defun cxrefs-history-update-lru (history)
  "Update current history HISTORY as newest history."
  (let ((ring (cxrefs-history-ring history))
	(pos (cxrefs-history-pos history)))
    (setf (cxrefs-history-pos history) 0)
    (when (not (ring-empty-p ring))
      (ring-insert ring (ring-remove ring pos)))))

(defun cxrefs-history-go-prev (history)
  "Go previous history on HISTORY and return it."
  (let ((ring (cxrefs-history-ring history))
	(prev-pos (1+ (cxrefs-history-pos history))))
    (if (> prev-pos (1- (ring-length ring)))
	nil
      (setf (cxrefs-history-pos history) prev-pos)
      (ring-ref ring prev-pos))))

(defun cxrefs-history-go-next (history)
  "Go next history on HISTORY and return it."
  (let ((ring (cxrefs-history-ring history))
	(next-pos (1- (cxrefs-history-pos history))))
    (if (< next-pos 0)
	nil
      (setf (cxrefs-history-pos history) next-pos)
      (ring-ref ring next-pos))))

(defun cxrefs-history-current (history)
  "Retrun current history on HISTORY."
  (let ((ring (cxrefs-history-ring history)))
    (if (ring-empty-p ring)
	nil
      (ring-ref ring (cxrefs-history-pos history)))))

;;; Marker history functions
(defun cxrefs-marker-kill-fn (marker)
  (set-marker marker nil))

(defun cxrefs-marker-make ()
  (cxrefs-history-make cxrefs-marker-length #'cxrefs-marker-kill-fn))

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
  (cxrefs-history-make cxrefs-selbuf-length #'kill-buffer))

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

;;; Context helpers
(cl-defstruct (cxrefs-ctx
	       (:constructor nil)	; no default
	       (:copier nil)
	       (:predicate nil)
	       (:constructor cxrefs-ctx-make (dir
					      &aux
					      (marker (cxrefs-marker-make))
					      (selbuf (cxrefs-selbuf-make)))))
  (dir dir :read-only t)
  backend
  process
  (marker nil :read-only t)
  (selbuf nil :read-only t)
  window-config
  buffer-list)

(defmacro cxrefs-define-backend (sym)
  (let* ((name (symbol-name sym))
	 (init-fn (intern (concat "cxrefs-" name "-init")))
	 (check-db-fn (intern (concat "cxrefs-" name "-check-db")))
	 (build-db-fn (intern (concat "cxrefs-" name "-build-db")))
	 (command-fn (intern (concat "cxrefs-" name "-command"))))
    `(add-to-list 'cxrefs-backends
		  (quote (,sym . (:init-fn ,init-fn
					   :check-db-fn ,check-db-fn
					   :build-db-fn ,build-db-fn
					   :command-fn ,command-fn))))))

(defvar cxrefs-backends nil
  "Cxrefs backend list.")

(defun cxrefs-ctx-backend-key (ctx)
  (car (cxrefs-ctx-backend ctx)))
(defun cxrefs-ctx-backend-fn (ctx sym)
  (plist-get (cdr (cxrefs-ctx-backend ctx)) sym))
(defun cxrefs-backend-init (ctx &optional option)
  (funcall (cxrefs-ctx-backend-fn ctx :init-fn) ctx option))
(defun cxrefs-backend-check-db (ctx)
  (funcall (cxrefs-ctx-backend-fn ctx :check-db-fn) ctx))
(defun cxrefs-backend-build-db (ctx)
  (funcall (cxrefs-ctx-backend-fn ctx :build-db-fn) ctx))
(defun cxrefs-backend-command (ctx cmd-type &optional string)
  (funcall (cxrefs-ctx-backend-fn ctx :command-fn) ctx cmd-type string))

(defun cxrefs-process-live-p (ctx)
  (let ((process (cxrefs-ctx-process ctx)))
    (and (processp process) (process-live-p process))))

(defun cxrefs-process-init (ctx &optional option)
  (unless (cxrefs-process-live-p ctx)
    (setf (cxrefs-ctx-process ctx) (cxrefs-backend-init ctx option))))

(defun cxrefs-target-buffer-add (ctx target-buffer)
  (let ((buffers (cxrefs-ctx-buffer-list ctx)))
    (when (not (member target-buffer buffers))
      ;; Cleanup killed buffers
      (setq buffers (delete nil (mapcar (lambda (buf)
					  (and (buffer-live-p buf) buf))
					buffers)))
      ;; Add target-buffer
      (setf (cxrefs-ctx-buffer-list ctx) (cons target-buffer buffers)))))

(defun cxrefs-target-buffer-kill (ctx)
  (when cxrefs-kill-buffers-on-quit
    (let ((live-buffers (delete nil (mapcar (lambda (buf)
					      (and (buffer-live-p buf)
						   (not (buffer-modified-p buf))
						   buf))
					    (cxrefs-ctx-buffer-list ctx)))))
      (when (and live-buffers (if (eq cxrefs-kill-buffers-on-quit 'ask)
				  (y-or-n-p "Cxrefs: Kill opened buffers? ")
				t))
	(mapc #'kill-buffer live-buffers)))))

;; Utility functions
(cl-defstruct (cxrefs-location
	       (:constructor nil)	; no default
	       (:copier cxrefs-location-copy)
	       (:predicate nil)
	       (:constructor
		cxrefs-location-make (ctx
				      func
				      file
				      line
				      hint
				      &aux
				      (file
				       (cxrefs-relative-file-name ctx file))
				      (funclen (length func))
				      (line (if (stringp line)
						(string-to-number line)
					      line))
				      (str (format "%s:%d" file line))
				      (strlen (length str)))))
  (file nil :read-only t)
  func
  funclen
  (line nil :read-only t)
  (str nil :read-only t)
  (strlen nil :read-only t)
  (hint nil :read-only t)
  depth
  excluded)

(defun cxrefs-relative-file-name (ctx file)
  (let* ((basedir (cxrefs-ctx-dir ctx))
	 (absolute (expand-file-name file basedir)))
    (if (string-prefix-p basedir absolute)
	(file-relative-name absolute basedir)
      (identity absolute))))

(defun cxrefs-expand-file-name (ctx file)
  (let ((basedir (cxrefs-ctx-dir ctx)))
    (if (file-name-absolute-p file)
	(identity file)
      (concat basedir file))))

(defun cxrefs-xref-output (buffer ctx cmd-type string filter locations)
  (let ((exclude-info (list :func-len cxrefs-min-function-width
			    :loc-len cxrefs-min-location-width
			    :filter (nth 0 filter)))
	(include-info (list :func-len cxrefs-min-function-width
			    :loc-len cxrefs-min-location-width
			    :filter (nth 1 filter))))
    ;; Calculate maximum length for :func-len and :loc-len
    (dolist (loc locations)
      (let ((info (if (cxrefs-location-excluded loc) exclude-info include-info)))
	(dolist (prop '(:func-len :loc-len))
	  (let ((maxlen (max (plist-get info prop)
			     (if (eq prop :func-len)
				 (cxrefs-location-funclen loc)
			       (cxrefs-location-strlen loc)))))
	    (setq info (plist-put info prop maxlen))))))
    ;; Make format string
    (dolist (info (list exclude-info include-info))
      (let ((func-len-str (number-to-string (plist-get info :func-len)))
	    (loc-len-str (number-to-string (plist-get info :loc-len))))
	(setq info (plist-put info :format-str
			      (concat "%-" func-len-str "s"
				      " %-" loc-len-str "s"
				      " %s\n")))))
    ;; Start output
    (with-current-buffer buffer
      ;; Insert headers
      (insert "-*- mode: cxrefs-select -*-\n")
      (insert (format "Dir: %s\n" (cxrefs-ctx-dir ctx)))
      (insert (format "Backend: %s\n" (cxrefs-ctx-backend-key ctx)))
      (insert (format "Find[%s]: %s\n" cmd-type string))
      (when (plist-get exclude-info :filter)
	(insert (format "Exclude[%s]: %s\n"
			(if cxrefs-use-filter-regexp "regexp" "pattern")
			(plist-get exclude-info :filter))))
      (when (plist-get include-info :filter)
	(insert (format "Include[%s]: %s\n"
			(if cxrefs-use-filter-regexp "regexp" "pattern")
			(plist-get include-info :filter))))
      (insert "\n")
      (setq include-info (plist-put include-info :marker (point-marker)))
      (dolist (loc locations)
	(let ((info (if (cxrefs-location-excluded loc)
			exclude-info
		      include-info)))
	  (when (and (cxrefs-location-excluded loc)
		     (not (plist-get info :marker)))
	    (insert "\n")
	    (insert cxrefs-excluded-line-sep "\n")
	    (setq info (plist-put info :marker (point-marker))))
	  ;; Adjust point to insert string
	  (goto-char (plist-get info :marker))
	  ;; Insert xrefs
	  (insert-before-markers
	   (format (plist-get info :format-str)
		   (cxrefs-location-func loc)
		   (cxrefs-location-str loc)
		   (cxrefs-location-hint loc))))))
    ))

;; Cxrefs context management
(defvar cxrefs-context-hash (make-hash-table :test 'equal :weakness nil))
(defvar-local cxrefs-basedir nil)

(defun cxrefs-context-find (basedir)
  (gethash basedir cxrefs-context-hash))
(defun cxrefs-context-add (ctx)
  (puthash (cxrefs-ctx-dir ctx) ctx cxrefs-context-hash))
(defun cxrefs-context-del (ctx)
  (remhash (cxrefs-ctx-dir ctx) cxrefs-context-hash))

(defun cxrefs-basedir-list ()
  "Return the all cxrefs-basedir."
  (let (basedir-list)
    (maphash (lambda (_key ctx)
	       (push (cxrefs-ctx-dir ctx) basedir-list))
	     cxrefs-context-hash)
    basedir-list))

(defun cxrefs-context-current ()
  "Retrun the current cxrefs context."
  (cxrefs-context-find cxrefs-basedir))

(defun cxrefs-context-make (basedir)
  "Initialize the cxrefs context with BASEDIR."
  (cxrefs-context-add (cxrefs-ctx-make basedir)))

(defun cxrefs-context-delete (ctx)
  "Delete the cxrefs context CTX associated with BASEDIR."
  (when ctx
    (cxrefs-marker-reset (cxrefs-ctx-marker ctx))
    (cxrefs-selbuf-reset (cxrefs-ctx-selbuf ctx))
    (cxrefs-target-buffer-kill ctx)
    (cxrefs-context-del ctx)))

(defvar cxrefs-no-context-error "No associated context for cxrefs")
(defvar cxrefs-no-marker-error "No %s locations for cxrefs")
(defvar cxrefs-no-buffer-error "The marked buffer has been deleted")
(defvar cxrefs-no-select-buffer-error "No %s select buffer")

;; Marker history interface
(defun cxrefs-marker-go-next ()
  "Go next to where point was newer invoked."
  (interactive nil cxrefs-mode)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(user-error cxrefs-no-context-error)
      (let ((marker (cxrefs-history-go-next (cxrefs-ctx-marker ctx))))
	(unless marker
	  (user-error cxrefs-no-marker-error "next"))
	(switch-to-buffer (or (marker-buffer marker)
			      (user-error cxrefs-no-buffer-error)))
	(goto-char (marker-position marker))
	(run-hooks 'cxrefs-post-jump-hook)))))

(defun cxrefs-marker-go-prev ()
  "Go previous to where point was last invoked."
  (interactive nil cxrefs-mode)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(user-error cxrefs-no-context-error)
      (let ((history (cxrefs-ctx-marker ctx)))
	;; if this is top, replace it with current point
	(when (cxrefs-history-pos-top-p history)
	  (cxrefs-history-pop history)
	  (cxrefs-history-push history (point-marker)))
	;; go back previous marker
	(let ((marker (cxrefs-history-go-prev history)))
	  (unless marker
	    (user-error cxrefs-no-marker-error "previous"))
	  (switch-to-buffer (or (marker-buffer marker)
				(user-error cxrefs-no-buffer-error)))
	  (goto-char (marker-position marker))
	  (run-hooks 'cxrefs-post-jump-hook))))))

;; Select buffer history interface
(defun cxrefs-selbuf-go-next ()
  "Go next select buffer."
  (interactive nil cxrefs-select-mode)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(user-error cxrefs-no-context-error)
      (let ((next (cxrefs-history-go-next (cxrefs-ctx-selbuf ctx))))
	(if (not (bufferp next))
	    (user-error cxrefs-no-select-buffer-error "next"))
	(switch-to-buffer next)))))

(defun cxrefs-selbuf-go-prev ()
  "Go previous select buffer."
  (interactive nil cxrefs-select-mode)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(user-error cxrefs-no-context-error)
      (let ((prev (cxrefs-history-go-prev (cxrefs-ctx-selbuf ctx))))
	(if (not (bufferp prev))
	    (user-error cxrefs-no-select-buffer-error "previous"))
	(switch-to-buffer prev)))))

;; Hooks
(defvar cxrefs-show-xref-select-hook nil)
(defvar cxrefs-back-and-next-select-hook nil)
(defvar cxrefs-file-not-found-hook nil)
(defvar cxrefs-select-mode-hook nil)

(defun cxrefs-ask-basedir ()
  "Tell tags commands to use tag table file FILE."
  (let* ((file-name-history (cxrefs-basedir-list))
	 (basedir (read-directory-name "Locate root directory: "
				       default-directory default-directory t)))
    basedir))

(defun cxrefs-find-basedir ()
  "Find basedir of ancestor instance for current dir."
  (let ((basedir-list (cxrefs-basedir-list)))
    (when basedir-list
      (let ((curdir default-directory)
	    prevdir)
	(catch 'loop
	  (while (not (equal prevdir curdir))
	    (setq curdir (file-name-as-directory curdir))
	    ;; match with exists instance?
	    (let ((basedir (car (member curdir basedir-list))))
	      (when basedir
		(throw 'loop basedir)))
	    (setq prevdir curdir)
	    ;; climb up the directory hierarchy
	    (setq curdir (file-name-directory (directory-file-name curdir))))))
      )))

(defun cxrefs-get-basedir (&optional try-exists)
  "Get basedir of ctx.

If TRY-EXISTS is t, try to use an existent ctx if there is cxrefs
instance for ancestor dir of current dir.

If not exists, ask to user."
  (if cxrefs-basedir
      cxrefs-basedir
    (let ((basedir (or (and try-exists (cxrefs-find-basedir))
		       (cxrefs-ask-basedir))))
      ;; Make sure the last char of "root" is `/'
      (file-name-as-directory (expand-file-name basedir)))))

;; Find exists databases for each backends. If no database, use default.
(defun cxrefs-select-backend (ctx)
  (let (result)
    (dolist (backend cxrefs-backends result)
      (unless result
	(setf (cxrefs-ctx-backend ctx) backend)
	(when (cxrefs-backend-check-db ctx)
	  (setq result backend))))
    (unless result
      (setq result (assoc cxrefs-backend-default cxrefs-backends)))
    (message "Use %s backend." (car result))
    result))

(defun cxrefs-check-and-build-db ()
  (let ((ctx (cxrefs-context-current)))
    (unless (cxrefs-ctx-backend ctx)
      (setf (cxrefs-ctx-backend ctx) (cxrefs-select-backend ctx)))
    (unless (cxrefs-backend-check-db ctx)
      (cxrefs-backend-build-db ctx))))

(defun cxrefs-check-tags-table ()
  (unless cxrefs-basedir
    (setq cxrefs-basedir (cxrefs-get-basedir 'try-exists)))
  (unless (cxrefs-context-current)
    (cxrefs-context-make cxrefs-basedir))
  (cxrefs-check-and-build-db))

;; Mark if matches exclude regexp, but not matches include regexp
(defun cxrefs-xref-mark-exclude (filter locations)
  (let ((exclude (nth 0 filter))
	(include (nth 1 filter)))
    (if (not exclude)
	locations
      (mapcar (lambda (loc)
		(let* ((file (cxrefs-location-file loc)))
		  (when (and (not (and include (string-match include file)))
			     (string-match exclude file))
		    (setf (cxrefs-location-excluded loc) t))
		  loc))
	      locations))))

(defun cxrefs-backend-command-with-mark (ctx cmd-type string filter)
  (cxrefs-xref-mark-exclude filter
			    (cxrefs-backend-command ctx cmd-type string)))

;; Make hierarchy by reverse order
(defun cxrefs-xref--hierarchy (ctx cmd-type func whole depth max-depth
				   arrow filter locations-cache)
  (let ((prefix (format "%s%s" (make-string depth ? ) arrow))
	(locations (gethash func locations-cache)))
    ;; If didn't hit locations-cache, ask to backend, then cache it
    (when (null locations)
      (setq locations
	    (cxrefs-backend-command-with-mark ctx cmd-type func filter))
      (puthash func locations locations-cache))
    (dolist (loc locations whole)
      ;; Copy loc to avoid modify cached loc
      (let* ((loc (cxrefs-location-copy loc))
	     (next-func (cxrefs-location-func loc)))
	;; Add hierarchy annotation to function
	(let ((prefix-func (format "%s %s" prefix next-func)))
	  (setf (cxrefs-location-func loc)  prefix-func)
	  (setf (cxrefs-location-funclen loc) (length prefix-func)))
	(setf (cxrefs-location-depth loc) depth)
	;; Make whole list by reverse order
	(push loc whole)
	(when (and (< depth max-depth) (not (cxrefs-location-excluded loc)))
	  (setq whole (cxrefs-xref--hierarchy ctx cmd-type next-func whole
					      (1+ depth) max-depth
					      arrow filter locations-cache)))))
    ))
(defun cxrefs-xref-hierarchy (ctx cmd-type string filter args)
  (defconst cxrefs-cmd-type-table '((caller-hierarchy caller "<-")
				    (callee-hierarchy callee "->")))
  (let* ((max-depth (nth 0 args))
	 (type (nth 1 (assoc cmd-type cxrefs-cmd-type-table)))
	 (arrow (nth 2 (assoc cmd-type cxrefs-cmd-type-table)))
	 (locations-cache (make-hash-table :test #'equal)))
    (nreverse (cxrefs-xref--hierarchy ctx type string nil 0 max-depth
				      arrow filter locations-cache))))

(defun cxrefs-xref-command (ctx cmd-type string filter args)
  ;; convert pattern to regexp
  (when (and filter (not cxrefs-use-filter-regexp))
    (setq filter (mapcar 'cxrefs-pattern-to-regexp filter)))
  (cond
   ((or (eq cmd-type 'caller-hierarchy) (eq cmd-type 'callee-hierarchy))
    (cxrefs-xref-hierarchy ctx cmd-type string filter args))
   (t
    (cxrefs-backend-command-with-mark ctx cmd-type string filter))))

(defun cxrefs-show-xref-select (cmd-type string filter &rest args)
  (cxrefs-check-tags-table)
  (let* ((ctx (cxrefs-context-current))
	 (select-buffer-name (format "*Cxrefs (%s)*" string))
	 (old-buffer (cxrefs-selbuf-current (cxrefs-ctx-selbuf ctx)))
	 (buffer (generate-new-buffer select-buffer-name)))
    (buffer-disable-undo buffer)
    (setf (cxrefs-ctx-window-config ctx) (current-window-configuration))
    (cxrefs-marker-push (cxrefs-ctx-marker ctx))
    (cxrefs-selbuf-push (cxrefs-ctx-selbuf ctx) buffer)
    (if (and (bufferp old-buffer) (get-buffer-window old-buffer))
	;; if there is select-buffer, use it
	(progn
	  (select-window (get-buffer-window old-buffer))
	  (switch-to-buffer buffer))
      (pop-to-buffer buffer))
    ;; Insert cxrefs-select output
    (let ((locations (cxrefs-xref-command ctx cmd-type string filter args)))
      (cxrefs-xref-output buffer ctx cmd-type string filter locations))
    (goto-char (point-min))
    (cxrefs-select-mode)
    (run-hooks 'cxrefs-show-xref-select-hook)))

(defun cxrefs-read-string (prompt-prefix type)
  (let ((default (thing-at-point type)))
    (read-string (format-prompt prompt-prefix default)
		 nil 'cxrefs-string-history default)))

(defun cxrefs-read-exclude ()
  (let ((string (read-regexp
		 (format "Exclude files matching this %s: "
			 (if cxrefs-use-filter-regexp "regexp" "pattern"))
		 nil 'cxrefs-exclude-history)))
    (if (string= string "")
	nil
      string)))

(defun cxrefs-read-include ()
  (let ((string (read-regexp
		 (format"Include files matching this %s: "
			(if cxrefs-use-filter-regexp "regexp" "pattern"))
		 nil 'cxrefs-include-history)))
    (if (string= string "")
	nil
      string)))

(defun cxrefs-read-filter ()
  (let ((exclude (cxrefs-read-exclude)))
    (if (not exclude)
	nil
      (list exclude (cxrefs-read-include)))))

(defun cxrefs-read-depth ()
  ;; Don't add to history
  (let ((history-add-new-input nil))
    (read-number "Depth: " cxrefs-hierarchy-depth)))

(defun cxrefs-find-definition (string filter)
  "Query the definition of the given STRING.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find this definition" 'symbol)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'define string filter))

(defun cxrefs-find-symbol (string filter)
  "Find this symbol STRING exclude filter in FILTER.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find this symbol" 'symbol)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'symbol string filter))

(defun cxrefs-find-callee (string filter)
  "Find callee of symbol STRING exclude filter in FILTER.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find callee of this" 'symbol)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'callee string filter))

(defun cxrefs-find-caller (string filter)
  "Find caller of symbol STRING exclude filter in FILTER.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find caller of this" 'symbol)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'caller string filter))

(defun cxrefs-find-text (string filter)
  "Find this text STRING exclude filter in FILTER.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find this text" 'text)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'text string filter))

(defun cxrefs-find-grep (string filter)
  "Find this grep pattern STRING.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find this grep pattern" 'text)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'grep string filter))

(defun cxrefs-find-egrep (string filter)
  "Find this egrep pattern STRING.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find this egrep pattern" 'text)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'egrep string filter))

(defun cxrefs-find-file (string filter)
  "Find this file STRING.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find this file" 'filename)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'file string filter))

(defun cxrefs-find-includer (string filter)
  "Find files #including this file STRING.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list
		(cxrefs-read-string "Find files #including this" 'filename)
		(and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'includer string filter))

(defun cxrefs-find-assign (string filter)
  "Find assignments to this symbol STRING.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find assignments to this" 'symbol)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'assign string filter))

(defun cxrefs-toggle-case ()
  "Toggle ignore/use letter case."
  (interactive nil cxrefs-mode cxrefs-select-mode)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(user-error cxrefs-no-context-error)
      (cxrefs-backend-command ctx 'toggle-case)
      (message "Toggle case cxrefs"))))

(defun cxrefs-rebuild ()
  "Rebuild cross reference database."
  (interactive nil cxrefs-mode cxrefs-select-mode)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(user-error cxrefs-no-context-error)
      (cxrefs-backend-command ctx 'rebuild)
      (message "Rebuild cxrefs"))))

(defun cxrefs-quit ()
  "Quit backend and context."
  (interactive nil cxrefs-mode cxrefs-select-mode)
  (let ((ctx (cxrefs-context-current)))
    (when ctx
      (cxrefs-backend-command ctx 'quit)
      (cxrefs-context-delete ctx)))
  (setq cxrefs-basedir nil)
  (message "Quit cxrefs"))

(defun cxrefs-set-basedir ()
  "Set Cxrefs basedir.
The purpose is to set basedir even if there is the instance for
ancestor dir."
  (interactive nil cxrefs-mode cxrefs-select-mode)
  (when cxrefs-basedir
    (cxrefs-quit))
  (setq cxrefs-basedir (cxrefs-get-basedir))
  (cxrefs-check-tags-table))

(defun cxrefs-find-caller-hierarchy (string depth filter)
  "Find hierarchical callers of STRING until DEPTH deep caller.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find hierarchical callers" 'symbol)
		     (cxrefs-read-depth)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'caller-hierarchy string filter depth))

(defun cxrefs-find-callee-hierarchy (string depth filter)
  "Find hierarchical callee of STRING until DEPTH deep callee.
A prefix argument to set filter glob FILTER to exclude specified files."
  (interactive (list (cxrefs-read-string "Find hierarchical callees" 'symbol)
		     (cxrefs-read-depth)
		     (and current-prefix-arg (cxrefs-read-filter)))
	       cxrefs-mode cxrefs-select-mode)
  (cxrefs-show-xref-select 'callee-hierarchy string filter depth))

(defun cxrefs-back-and-next-select ()
  "Return to the cxrefs-select buffer and advance the cursor by one line."
  (interactive nil cxrefs-mode)
  (let ((ctx (cxrefs-context-current)))
    (if (not ctx)
	(user-error cxrefs-no-context-error)
      (let ((buffer (cxrefs-selbuf-current (cxrefs-ctx-selbuf ctx))))
	(if (not (bufferp buffer))
	    (user-error cxrefs-no-select-buffer-error "associated")
	  (setf (cxrefs-ctx-window-config ctx) (current-window-configuration))
	  (pop-to-buffer buffer t)))
      (cxrefs-select-next-line)
      (run-hooks 'cxrefs-back-and-next-select-hook))))

(defvar cxrefs-mode-map
  (let ((map (make-sparse-keymap)))
    ;; for xref and old etags
    (define-key map "\M-*"	'cxrefs-marker-go-prev)
    (define-key map "\M-,"	'cxrefs-marker-go-prev)
    (define-key map "\M-."	'cxrefs-find-definition)
;    (define-key map "\C-\M-."	'cxrefs-find-symbol)
    (define-key map "\M-?"	'cxrefs-find-caller)
    ;; cxrefs key binds
    (define-key map "\M-N"	'cxrefs-marker-go-next)
    (define-key map "\M-P"	'cxrefs-marker-go-prev)
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

;; cxrefs-select output parser
(defvar cxrefs-select-output-basedir
  "^Dir: \\(.*\\)$")
(defvar cxrefs-select-output-backend
  "^Backend: \\(.*\\)$")
(defvar cxrefs-output-line-regexp
  "^\\( *\\(->\\|<-\\) \\)?\\(.+?\\) +\\(.+?\\):\\([0-9]+\\) +\\(.*\\)$"
  "This regular expression is used to recognize valid reference lines.")
(defvar cxrefs-output-func-place 3
  "Position number of `cxrefs-output-line-regexp' which locates function name.")
(defvar cxrefs-output-file-place 4
  "Position number of `cxrefs-output-line-regexp' which locates filename.")
(defvar cxrefs-output-line-place 5
  "Position number of `cxrefs-output-line-regexp' which locates line number.")
(defvar cxrefs-output-hint-place 6
  "Position number of `cxrefs-output-line-regexp' which locates hint string.")

(defun cxrefs-toggle-auto-preview (&optional arg)
  "Toggle auto preview.

If called interactively, toggle ‘auto preview’.  If the prefix
argument is positive, enable the auto preview, and if it is zero
or negative, disable the auto preview.

If called from Lisp, toggle the mode if ARG is ‘toggle’.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the
mode if ARG is a negative number."
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg)
                       'toggle))
	       cxrefs-mode cxrefs-select-mode)
  (let* ((cur (member 'cxrefs-select-preview cxrefs-post-select-hook))
	 (enable (cond ((eq arg 'toggle)
			(not cur))
		       ((and (numberp arg) (< arg 1))
			nil)
		       (t
			t))))
    (if enable
	(add-hook 'cxrefs-post-select-hook #'cxrefs-select-preview)
      (remove-hook 'cxrefs-post-select-hook #'cxrefs-select-preview))
    (message "%s auto preview" (if enable "Enabled" "Disabled"))))

(defun cxrefs-select-next-line (&optional count)
  "Cxrefs select mode next line.
Move cursor vertically down COUNT lines."
  (interactive nil cxrefs-select-mode)
  (end-of-line)
  (re-search-forward cxrefs-output-line-regexp nil t count)
  (beginning-of-line)
  (run-hooks 'cxrefs-post-select-hook))

(defun cxrefs-select-previous-line (&optional count)
  "Cxrefs select mode previous line.
Move cursor vertically up COUNT lines."
  (interactive nil cxrefs-select-mode)
  (beginning-of-line)
  (re-search-backward cxrefs-output-line-regexp nil t count)
  (beginning-of-line)
  (run-hooks 'cxrefs-post-select-hook))

(defun cxrefs-depth-info ()
  (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
	 (match (string-match "\\`\\(\\( *\\)\\(<-\\|->\\)\\) " line))
	 (prefix (and match (match-string 1 line)))
	 (depth (and match (length (match-string 2 line))))
	 (arrow (and match (match-string 3 line))))
    (when match
      (list prefix depth arrow))))

(defun cxrefs-select-depth-next-line (&optional count)
  "Cxrefs select mode next line on same depth and hierarchy.
Move cursor vertically down COUNT lines on same depth and hierarchy."
  (interactive nil cxrefs-select-mode)
  (let ((info (cxrefs-depth-info)))
    (when info
      (end-of-line)
      (let* ((prefix (nth 0 info))
	     (depth (nth 1 info))
	     (arrow (nth 2 info))
	     (limit (if (= depth 0)
			nil
		      (save-excursion
			;; Find end of this hierarchy
			(let ((re (format "^ \\{,%d\\}%s " (1- depth) arrow)))
			  (re-search-forward re nil t))))))
	(re-search-forward (concat "^" prefix) limit t count))
      (beginning-of-line))))

(defun cxrefs-select-depth-previous-line (&optional count)
  "Cxrefs select mode previous line on same depth and hierarchy.
Move cursor vertically up COUNT lines on same depth and hierarchy."
  (interactive nil cxrefs-select-mode)
  (let ((info (cxrefs-depth-info)))
    (when info
      (beginning-of-line)
      (let* ((prefix (nth 0 info))
	     (depth (nth 1 info))
	     (arrow (nth 2 info))
	     (limit (if (= depth 0)
			nil
		      (save-excursion
			;; Find end of this hierarchy
			(let ((re (format "^ \\{,%d\\}%s " (1- depth) arrow)))
			  (re-search-backward re nil t))))))
	(re-search-backward (concat "^" prefix) limit t count))
      (beginning-of-line))))

(defun cxrefs-file-not-found ()
  (if cxrefs-file-not-found-hook
      (run-hooks 'cxrefs-file-not-found-hook)
    (kill-buffer (get-file-buffer (buffer-file-name)))
    (error "Cxrefs can't find file %s" (buffer-file-name))))

;; FIXME: cscope removes some whitespaces from hint. So it may not
;; match to target line even if no change actually.
(defun cxrefs-goto-target-line (line hint)
  (goto-char (point-min))
  (forward-line (1- line))
  (when (and (< 0 cxrefs-fuzzy-target-lines)
	     (stringp hint)
	     (not (string-match-p "^\\s-*$" hint))
	     ;; ignore too short hint
	     (> (length hint) 2))
    ;; Fuzzy match target line slightly out of dated
    (let ((startpos (point))
	  (re (regexp-quote hint))
	  ;; To accept small change, ignore whitespace changes.
	  ;; FIXME: is there better way?
	  (search-spaces-regexp "\\s-+")
	  (case-fold-search nil))
      ;; FIXME: usually a user would want to skip comment lines
      (when (not (looking-at-p (concat "\\s-*" re)))
	(let (fpos bpos bound)
	  ;; Backward search
	  (goto-char startpos)
	  (setq bound
		(line-beginning-position (- (1- cxrefs-fuzzy-target-lines))))
	  (setq bpos (re-search-backward re bound t))
	  ;; Forward search
	  (goto-char startpos)
	  (setq bound
		(line-beginning-position (1+ cxrefs-fuzzy-target-lines)))
	  (setq fpos (re-search-forward re bound t))
	  ;; Use near position from startpos
	  (when (and bpos fpos (< (- startpos bpos) (- fpos startpos)))
	    (setq fpos bpos))
;	  (when (and (null fpos) (null bpos))
;	    (message "Cxrefs: Hint not found. Maybe tag is out of date" hint))
;	  (when (or fpos bpos)
;	    (goto-char (or fpos bpos))
;	    (message "Cxrefs: Adjust %s => %s"
;		     startpos (line-beginning-position)))
	  (goto-char (or fpos bpos startpos)))))
    (beginning-of-line)))

(defun cxrefs-select-interpret-line (&optional preview)
  "Parse the line under the cursor as a cxrefs output reference line.
If PREVIEW is non-nil, show window without selecting."
  (interactive "P" cxrefs-select-mode)
  (beginning-of-line)
  (if (not (looking-at cxrefs-output-line-regexp))
      (error "Cxrefs Line not understood as a cxrefs reference line")
    (let* ((select-mode-window (get-buffer-window (current-buffer)))
	   (ctx (cxrefs-context-current))
	   (func (match-string cxrefs-output-func-place))
	   (file (match-string cxrefs-output-file-place))
	   (line (match-string cxrefs-output-line-place))
	   (hint (match-string cxrefs-output-hint-place))
	   (find-file-not-found-functions (list 'cxrefs-file-not-found))
	   (path (cxrefs-expand-file-name ctx file))
	   (exists-buffer (get-file-buffer path))
	   target-buffer target-window)
      (setq target-buffer (find-file-other-window path))
      ;; Remember newly opened buffer
      (when (and (not exists-buffer) target-buffer)
	(cxrefs-target-buffer-add ctx target-buffer))
      ;; Inherit cxrefs-basedir
      (setq cxrefs-basedir (cxrefs-ctx-dir ctx))
      (let ((history (cxrefs-ctx-selbuf ctx)))
	(when history
	  (cxrefs-selbuf-update-lru history)))
      (cxrefs-mode 1)
      ;; Change point to target line
      (message "Cxrefs Function: %s" func)
      (cxrefs-goto-target-line (string-to-number line) hint)
      (setq target-window (selected-window))
      (if preview
	  (select-window select-mode-window)
	(delete-other-windows))
      (with-selected-window target-window
	(run-hooks 'cxrefs-post-jump-hook)))))

(defun cxrefs-select-preview ()
  "Call `cxrefs-select-interpret-line' without selecting other window."
  (interactive nil cxrefs-select-mode)
  (cxrefs-select-interpret-line t))

(defun cxrefs-show-hide-excluded (show)
  (if show
      (widen)
    (save-excursion
      (goto-char (point-min))
      (let* ((regexp (concat "^" cxrefs-excluded-line-sep "$"))
	     (end (re-search-forward regexp nil t)))
	(when end
	  (narrow-to-region (point-min) end))))))

(defun cxrefs-toggle-show-excluded ()
  "Toggle show/hide excluded lines."
  (interactive nil cxrefs-select-mode)
  (let ((show (buffer-narrowed-p)))
    (cxrefs-show-hide-excluded show)
    (message "Cxrefs %s excluded lines" (if show "show" "hide"))))

(defun cxrefs-select-quit ()
  "Quit select-mode, then restore window configuration."
  (interactive nil cxrefs-select-mode)
  (let ((ctx (cxrefs-context-current)))
    (when ctx
      (let ((config (cxrefs-ctx-window-config ctx)))
	(when (window-configuration-p config)
	  (set-window-configuration config))))))

(defun cxrefs-select-read-context ()
  (goto-char (point-min))
  ;; Set basdir
  (re-search-forward cxrefs-select-output-basedir nil nil)
  (setq cxrefs-basedir (match-string 1))
  ;; Set backend if need
  (re-search-forward cxrefs-select-output-backend nil nil)
  (let ((backend-sym (intern (match-string 1))))
    (when (not (cxrefs-context-current))
      (cxrefs-context-make cxrefs-basedir))
    (let ((ctx (cxrefs-context-current)))
      (when (not (cxrefs-ctx-backend ctx))
	(setf (cxrefs-ctx-backend ctx) (assoc backend-sym cxrefs-backends))))
    ))

(defvar cxrefs-select-font-lock-keywords
  `((,cxrefs-select-output-basedir (1 'font-lock-keyword-face))
    (,cxrefs-select-output-backend (1 'font-lock-string-face))
    ("^Find\\[.+?\\]: \\(.*\\)$" (1 'font-lock-string-face))
    ("^Exclude\\[.+?\\]: \\(.*\\)$" (1 'font-lock-string-face))
    ("^Include\\[.+?\\]: \\(.*\\)$" (1 'font-lock-string-face))
    (,cxrefs-output-line-regexp
     (,cxrefs-output-func-place 'font-lock-function-name-face)
     (,cxrefs-output-file-place 'font-lock-keyword-face)
     (,cxrefs-output-line-place 'font-lock-keyword-face))))

(defconst cxrefs-select-font-lock-defaults
  '(cxrefs-select-font-lock-keywords t nil nil))

(defvar cxrefs-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cxrefs-mode-map)
    ;; for xref and old etags
    (define-key map "."		'cxrefs-find-definition)
    ;; cxrefs key binds
    (define-key map "\M-n"	'cxrefs-selbuf-go-next)
    (define-key map "\M-p"	'cxrefs-selbuf-go-prev)
    (define-key map "n"		'cxrefs-select-next-line)
    (define-key map "p"		'cxrefs-select-previous-line)
    (define-key map "N"		'cxrefs-select-depth-next-line)
    (define-key map "P"		'cxrefs-select-depth-previous-line)
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
    (define-key map "\C-t"	'cxrefs-toggle-auto-preview)
    (define-key map "E"		'cxrefs-toggle-show-excluded)
    (define-key map "q"		'cxrefs-select-quit)
    map)
  "Keymap used in Cxrefs select mode.")

;;;###autoload
(define-derived-mode cxrefs-select-mode special-mode "Cxrefs-Select"
  "Major mode for choosing a tag from tags list.
Turning on Cxrefs-Select mode calls the value of the variable"
  :interactive nil
  (setq-local font-lock-defaults cxrefs-select-font-lock-defaults)
  (setq truncate-lines t)
  (cxrefs-select-read-context)
  (cxrefs-show-hide-excluded cxrefs-show-excluded-lines)
  (goto-char (point-min))
  (cxrefs-select-next-line))

;;; Hooks
(defun cxrefs-highlight-position ()
  (when (fboundp 'pulse-momentary-highlight-one-line)
    (pulse-momentary-highlight-one-line (point) 'next-error)))

;;; Backends
(defgroup cxrefs-cscope nil
  "Cxrefs cscope backend."
  :version "25.1"
  :group 'cxrefs-cscope)

(defcustom cxrefs-cscope-program "cscope"
  "The name of the cscope executable."
  :type 'string)

(defmacro cxrefs-cscope-build-files ()
  `(concat "find -L . -type f -name '*.[chlyS]'"
	   ,@(mapcar (lambda (ext)
		       (concat " -o -name '*." ext "'"))
		     '("cc" "cpp" "cxx" "c++" "hh" "hpp" "hxx" "h++"))
	   " > cscope.files"))

(defcustom cxrefs-cscope-build-files-history
  `("cscope-linux.pl"
    ,(cxrefs-cscope-build-files)
    "cscope-freebsd.pl"
    "cscope-glibc.pl")
  "*Command for creating cscope.file."
  :type '(repeat string))

(defvar cxrefs-cscope-command-table
  '((symbol		. "0") (define		. "1")
    (callee		. "2") (caller		. "3")
    (text		. "4") (grep		. "5")
    (egrep		. "6") (file		. "7")
    (includer		. "8") (assign		. "9")
    (all		. "a") (toggle-case	. "c")
    (rebuild		. nil) (quit		. nil)))

(defun cxrefs-cscope-check-db (ctx)
  (file-exists-p (concat (cxrefs-ctx-dir ctx) "cscope.out")))

(defun cxrefs-cscope-wait-prompt (process)
  (while (and (processp process) (eq (process-status process) 'run)
	      (goto-char (point-min))
	      (not (re-search-forward "^>> $" nil t)))
    (accept-process-output process)))

(defun cxrefs-cscope-init (ctx &optional option)
  (let ((process-connection-type nil) ; use a pipe
	(default-directory (cxrefs-ctx-dir ctx))
	(program-args '("-l"))
	(buffer (generate-new-buffer "*Cscope-Process*"))
	process)
    (buffer-disable-undo buffer)
    (when (and (not (eq option 'rebuild)) (cxrefs-cscope-check-db ctx))
      (push "-d" program-args))
    (setq process (apply 'start-process "Cscope-Process" buffer
			 cxrefs-cscope-program program-args))
    (set-process-query-on-exit-flag process nil)
    (with-current-buffer (process-buffer process)
      (cxrefs-cscope-wait-prompt process))
    process))

(defun cxrefs-cscope-ask-files-command (basedir)
  "Return a command for creating cscope.files for BASEDIR."
  ;; If we don't know the command yet, ask user.
  (let ((cmd (read-string "Command for creating cscope.files: "
			  (car cxrefs-cscope-build-files-history)
			  '(cxrefs-cscope-build-files-history . 1))))
    (when (string= cmd "")
      (user-error "Invalid command for creating cscope.files"))
    (format cmd basedir)))

(defun cxrefs-cscope-build-db (ctx)
  "Build cscope.files with CTX."
  (let* ((basedir (cxrefs-ctx-dir ctx))
	 (command (cxrefs-cscope-ask-files-command basedir))
	 (default-directory basedir))
    ;; Build cscope.files
    (unless (= 0 (shell-command command))
      (error "Couldn't create cscope.files"))))

(defmacro with-cxrefs-cmd-output (ctx command &rest body)
  (declare (indent 2) (debug t))
  `(let ((process (cxrefs-ctx-process ,ctx)))
     (with-current-buffer (process-buffer process)
       (erase-buffer)
       (process-send-string process (concat ,command "\n"))
       (cxrefs-cscope-wait-prompt process)
       (goto-char (point-min))
       ,@body)))

(defun cxrefs-cscope-make-locations (ctx command)
  (with-cxrefs-cmd-output ctx command
    (let (locations)
      (while (re-search-forward
	      "^\\(.+?\\) \\([^ \t]+?\\) \\([0-9]+?\\) \\(.*\\)" nil t)
	(let ((file (match-string 1))
	      (func (match-string 2))
	      (line (match-string 3))
	      (hint (match-string 4)))
	  (push (cxrefs-location-make ctx func file line hint) locations)))
      (nreverse locations))))

(defun cxrefs-cscope-command (ctx cmd-type &optional string)
  (let ((cmd (cdr (assoc cmd-type cxrefs-cscope-command-table))))
    (cond
     ((stringp cmd)
      (cxrefs-process-init ctx)
      (cxrefs-cscope-make-locations ctx (concat cmd string)))
     ((eq cmd-type 'rebuild)
      ;; Quit if alive (cscope doesn't reload inverted index,
      ;; i.e. built with "-q" option). So restart cscope itself.
      (when (and (eq (cxrefs-ctx-backend-key ctx) 'cscope)
		 (cxrefs-process-live-p ctx))
	(with-cxrefs-cmd-output ctx "q"))
      ;; Then rebuild
      (cxrefs-process-init ctx 'rebuild)
      (with-cxrefs-cmd-output ctx "r"))
     ((eq cmd-type 'quit)
      (let ((process (cxrefs-ctx-process ctx)))
	(when (processp process)
	  (with-cxrefs-cmd-output ctx "q")
	  (when (process-buffer process)
	    (kill-buffer (process-buffer process)))))
      (setf (cxrefs-ctx-process ctx) nil)))
    ))

(cxrefs-define-backend cscope)

(defgroup cxrefs-gtags nil
  "Cxrefs gtags backend."
  :version "25.1"
  :group 'cxrefs-gtags)

(defcustom cxrefs-gtags-program "gtags-cscope"
  "The name of the gtags executable."
  :type 'string)

(defcustom cxrefs-gtags-build-program "gtags"
  "The name of the gtags executable to make database."
  :type 'string)

(defun cxrefs-gtags-check-db (ctx)
  (file-exists-p (concat (cxrefs-ctx-dir ctx) "GTAGS")))

(defun cxrefs-gtags-init (ctx &optional _option)
  (let ((process-connection-type nil) ; use a pipe
	(default-directory (cxrefs-ctx-dir ctx))
	(buffer (generate-new-buffer "*Gtags-Process*"))
	process)
    (buffer-disable-undo buffer)
    (setq process (start-process "Gtags-Process" buffer
				 cxrefs-gtags-program "-l"))
    (set-process-query-on-exit-flag process nil)
    ;; Wait prompt
    (accept-process-output process)
    process))

(defun cxrefs-gtags-build-db (ctx)
  "Build GTAGS with CTX."
  (let* ((basedir (cxrefs-ctx-dir ctx))
	 (command cxrefs-gtags-build-program)
	 (default-directory basedir))
    ;; Build cscope.files
    (unless (= 0 (shell-command command))
      (error "Couldn't create cscope.files"))))

(defalias 'cxrefs-gtags-command 'cxrefs-cscope-command)

(cxrefs-define-backend gtags)

(provide 'cxrefs)
;;; cxrefs.el ends here
