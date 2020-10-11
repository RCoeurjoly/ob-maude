;;; ob-maude.el --- Babel Functions for Maude    -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2020 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org Babel support for evaluating Maude source code.
;;
;; We evaluate using the Maude interpreter.

;;; Requirements:

;; - maude-mode: https://sourceforge.net/projects/maude-mode/

;;; Code:
(require 'ob)
(require 'org-macs)
(require 'comint)

(declare-function inf-maude "ext:run-maude" (&optional arg))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("maude" . "maude"))

(defvar org-babel-default-header-args:maude
  '((:padlines . "no")))

(defconst org-babel-maude-eoe "\"org-babel-maude-eoe\"")

(defvar maude-prompt-regexp)

(defun org-babel-maude-execute (body params)
  "This function should only be called by `org-babel-execute:maude'"
  (let* ((tmp-src-file (org-babel-temp-file "Maude-src-" ".maude"))
         (cmdline (cdr (assq :cmdline params)))
         (cmdline (if cmdline (concat " " cmdline) ""))
         (flags (cdr (assq :flags params)))
         (flags (mapconcat #'identity
		           (if (listp flags)
                               flags
                             (list flags))
			   " "))
         (libs (org-babel-read
	        (or (cdr (assq :libs params))
	            (org-entry-get nil "libs" t))
	        nil))
         (libs (mapconcat #'identity
		          (if (listp libs) libs (list libs))
		          " ")))
    (with-temp-file tmp-src-file (insert body))
    (org-babel-eval
     (format "%s -o %s %s %s %s"
             org-babel-maude-compiler
	     tmp-bin-file
	     flags
	     (org-babel-process-file-name tmp-src-file)
	     libs)
     "")
    (let ((results (org-babel-eval (concat tmp-bin-file cmdline) "")))
      (when results
        (setq results (org-trim (org-remove-indentation results)))
        (org-babel-reassemble-table
         (org-babel-result-cond (cdr (assq :result-params params))
	   (org-babel-read results t)
	   (let ((tmp-file (org-babel-temp-file "Maude-")))
	     (with-temp-file tmp-file (insert results))
	     (org-babel-import-elisp-from-file tmp-file)))
         (org-babel-pick-name
	  (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
         (org-babel-pick-name
	  (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))))

(defun org-babel-interpret-maude (body params)
  (add-hook 'inferior-maude-hook
            (lambda ()
              (setq-local comint-prompt-regexp
                          maude-prompt-regexp)))
  (let* ((session (cdr (assq :session params)))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params
		     (org-babel-variable-assignments:maude params)))
         (session (org-babel-maude-initiate-session session params))
	 (comint-preoutput-filter-functions
	  (cons 'ansi-color-filter-apply comint-preoutput-filter-functions))
         (raw (org-babel-comint-with-output
		  (session org-babel-maude-eoe t full-body)
                (insert (org-trim full-body))
                (comint-send-input nil t)
                (insert org-babel-maude-eoe)
                (comint-send-input nil t)))
         (results (mapcar #'org-strip-quotes
			  (cdr (member org-babel-maude-eoe
                                       (reverse (mapcar #'org-trim raw)))))))
    (org-babel-reassemble-table
     (let ((result
            (pcase result-type
              (`output (mapconcat #'identity (reverse results) "\n"))
              (`value (car results)))))
       (org-babel-result-cond (cdr (assq :result-params params))
	 result ))
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colname-names params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rowname-names params))))))

(defun org-babel-execute:maude (body params)
  "Execute a block of Maude code."
	(org-babel-interpret-maude body params))

(defun org-babel-maude-initiate-session (&optional _session _params)
  "Initiate a maude session.
If there is not a current inferior-process-buffer in SESSION
then create one.  Return the initialized session."
  (or (get-buffer "*maude*")
      (save-window-excursion (run-maude) (sleep-for 0.25) (current-buffer))))

(defun org-babel-load-session:maude (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let* ((buffer (org-babel-prep-session:maude session params))
           (load-file (concat (org-babel-temp-file "maude-load-") ".maude")))
      (with-temp-buffer
        (insert body) (write-file load-file)
        (maude-mode) (inferior-maude-load-file))
      buffer)))

(defun org-babel-prep-session:maude (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (save-window-excursion
    (let ((buffer (org-babel-maude-initiate-session session)))
      (org-babel-comint-in-buffer buffer
      	(mapc (lambda (line)
		(insert line)
		(comint-send-input nil t))
	      (org-babel-variable-assignments:maude params)))
      (current-buffer))))

(defun org-babel-variable-assignments:maude (params)
  "Return list of maude statements assigning the block's variables."
  (mapcar (lambda (pair)
	    (format "let %s = %s"
		    (car pair)
		    (org-babel-maude-var-to-maude (cdr pair))))
	  (org-babel--get-vars params)))

(defun org-babel-maude-var-to-maude (var)
  "Convert an elisp value VAR into a maude variable.
The elisp VAR is converted to a string of maude source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-maude-var-to-maude var ", ") "]")
    (format "%S" var)))

(defvar org-export-copy-to-kill-ring)
(declare-function org-export-to-file "ox"
		  (backend file
			   &optional async subtreep visible-only body-only
			   ext-plist post-process))

(provide 'ob-maude)

;;; ob-maude.el ends here
