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

;; - maude-mode: http://www.iro.umontreal.ca/~monnier/elisp/#maude-mode
;; - (optionally) lhs2tex: http://people.cs.uu.nl/andres/lhs2tex/

;;; Code:
(require 'ob)
(require 'org-macs)
(require 'comint)

(declare-function inf-maude "ext:run-maude" (&optional arg))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("maude" . "hs"))

(defvar org-babel-default-header-args:maude
  '((:padlines . "no")))

(defvar org-babel-maude-eoe "*** org-babel-maude-eoe")

(defvar maude-prompt-regexp)

(defun org-babel-maude-execute (body params)
  "This function should only be called by `org-babel-execute:maude'"
  (let* ((tmp-src-file (org-babel-temp-file "Maude-src-" ".maude"))
         (tmp-bin-file
          (org-babel-process-file-name
           (org-babel-temp-file "Maude-bin-" org-babel-exeext)))
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
                (insert lol)
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
           (load-file (concat (org-babel-temp-file "maude-load-") ".hs")))
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
(defun org-babel-maude-export-to-lhs (&optional arg)
  "Export to a .lhs file with all maude code blocks escaped.
When called with a prefix argument the resulting
.lhs file will be exported to a .tex file.  This function will
create two new files, base-name.lhs and base-name.tex where
base-name is the name of the current Org file.

Note that all standard Babel literate programming
constructs (header arguments, no-web syntax etc...) are ignored."
  (interactive "P")
  (let* ((contents (buffer-string))
         (maude-regexp
          (concat "^\\([ \t]*\\)#\\+begin_src[ \t]maude*\\(.*\\)[\r\n]"
                  "\\([^\000]*?\\)[\r\n][ \t]*#\\+end_src.*"))
         (base-name (file-name-sans-extension (buffer-file-name)))
         (tmp-file (org-babel-temp-file "maude-"))
         (tmp-org-file (concat tmp-file ".org"))
         (tmp-tex-file (concat tmp-file ".tex"))
         (lhs-file (concat base-name ".lhs"))
         (tex-file (concat base-name ".tex"))
         (command (concat org-babel-maude-lhs2tex-command
			  " " (org-babel-process-file-name lhs-file)
			  " > " (org-babel-process-file-name tex-file)))
         (preserve-indentp org-src-preserve-indentation)
         indentation)
    ;; escape maude source-code blocks
    (with-temp-file tmp-org-file
      (insert contents)
      (goto-char (point-min))
      (while (re-search-forward maude-regexp nil t)
        (save-match-data (setq indentation (length (match-string 1))))
        (replace-match (save-match-data
                         (concat
                          "#+begin_export latex\n\\begin{code}\n"
                          (if (or preserve-indentp
                                  (string-match "-i" (match-string 2)))
                              (match-string 3)
                            (org-remove-indentation (match-string 3)))
                          "\n\\end{code}\n#+end_export\n"))
                       t t)
        (indent-code-rigidly (match-beginning 0) (match-end 0) indentation)))
    (save-excursion
      ;; export to latex w/org and save as .lhs
      (require 'ox-latex)
      (find-file tmp-org-file)
      ;; Ensure we do not clutter kill ring with incomplete results.
      (let (org-export-copy-to-kill-ring)
	(org-export-to-file 'latex tmp-tex-file))
      (kill-buffer nil)
      (delete-file tmp-org-file)
      (find-file tmp-tex-file)
      (goto-char (point-min)) (forward-line 2)
      (insert "%include polycode.fmt\n")
      ;; ensure all \begin/end{code} statements start at the first column
      (while (re-search-forward "^[ \t]+\\\\begin{code}[^\000]+\\\\end{code}" nil t)
        (replace-match (save-match-data (org-remove-indentation (match-string 0)))
                       t t))
      (setq contents (buffer-string))
      (save-buffer) (kill-buffer nil))
    (delete-file tmp-tex-file)
    ;; save org exported latex to a .lhs file
    (with-temp-file lhs-file (insert contents))
    (if (not arg)
        (find-file lhs-file)
      ;; process .lhs file with lhs2tex
      (message "running %s" command) (shell-command command) (find-file tex-file))))

(provide 'ob-maude)

;;; ob-maude.el ends here
