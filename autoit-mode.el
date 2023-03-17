;;; autoit-mode.el --- major mode for the AutoIT windows automation tool. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Constantin Kulikov

;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 0.1
;; Date: 2023/03/13 10:15:22
;; License: GPL either version 3 or any later version
;; Keywords: autoit major-mode
;; URL: https://github.com/Bad-ptr/autoit.el

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Support syntax highlighting and indentation for AutoIT language,
;; used in windows automation tool.

;;; Usage:

;; Just open and edit some *.au3 files

;; Installation:

;; M-x package-install-file RET autoit-mode.el RET


;;; Code:


;;(require 'cl-lib)

(defvar autoit-mode-hook nil)
(defvar autoit-mode      nil)


(defvar autoit-font-lock-keywords
  (list
   '("\\<\\(Or\\|And\\|Switch\\|Case\\|EndSwitch\\|Select\\|EndSelect\\|ContinueCase\\|If\\|Then\\|Else\\|ElseIf\\|EndIf\\|ExitLoop\\|ContinueLoop\\|While\\|WEnd\\|Do\\|Until\\|For\\|To\\|Step\\|Next\\|Func\\|EndFunc\\|Return\\|Exit\\|With\\|EndWith\\)\\>" . font-lock-keyword-face)
   '("\\<\\(Const\\|Dim\\|Global\\|Local\\)\\>" . font-lock-keyword-face)
   '("@\\w*" . font-lock-constant-face)
   '("\\<\\(DirCreate\\|FileClose\\|FileCopy\\|FileDelete\\|FileExists\\|FileFindFirstFile\\|FileFindNextFile\\|FileGetAttrib\\|FileGetSize\\|FileOpen\\|FileReadLine\\|FileWriteLine\\)\\>" . font-lock-builtin-face)
   '("\\<\\(WinActivate\\|WinClose\\|WinExists\\|WinGetHandle\\|WinGetPos\\|WinKill\\|WinMenuSelectItemByHandle\\|WinSetState\\)\\>" . font-lock-builtin-face)
   '("\\<\\(GUICtrlCreateButton\\|GUICtrlCreateEdit\\|GUICtrlCreateInput\\|GUICtrlCreateLabel\\|GUICtrlCreateListView\\|GUICtrlCreateMenu\\|GUICtrlCreateTreeView\\)\\>" . font-lock-function-name-face)
   '("\\<\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)\\>\\s-*(" 1 font-lock-function-name-face)
   '("\\$[a-zA-Z_]+[a-zA-Z0-9_]*" . font-lock-variable-name-face)
   ;;'("'.*'" . font-lock-string-face)
   ;;'(";.*$" . font-lock-comment-face)
   )
  "Default highlighting expressions for AutoIT mode")

(defvar autoit-mode-syntax-table
  (let ((st (make-syntax-table)))
	(modify-syntax-entry ?_  "w"   st)
	(modify-syntax-entry ?@  "w"   st)
	(modify-syntax-entry ?$  "w"   st)
	(modify-syntax-entry ?#  "<"   st)
	(modify-syntax-entry ?\; "<"   st)
	(modify-syntax-entry ?\n "> ." st)
	(modify-syntax-entry ?'  "\"'" st)
	(modify-syntax-entry ?\\ "_"   st)
	st)
  "Syntax table for AutoIT mode")




;; Attempt at SMIE BEGIN (

;; (require 'smie)

;; (defvar autoit-indent-offset 1)
;; (defvar autoit-smie-grammar
;;   (smie-prec2->grammar
;;    (smie-bnf->prec2
;; 	'((id)
;; 	  (inst
;; 	   ("If" exps "Then" insts "EndIf")
;; 	   ("If" exps "Then"
;; 		insts
;; 		"EndIf")
;; 	   ("If" exps "Then"
;; 		insts
;; 		"Else"
;; 		insts
;; 		"EndIf")
;; 	   ("If" exps "Then"
;; 		insts
;; 		"ElseIf" exps "Then"
;; 		insts
;; 		"EndIf")
;; 	   ("If" exps "Then"
;; 		insts
;; 		"ElseIf" exps "Then"
;; 		insts
;; 		"Else"
;; 		insts
;; 		"EndIf")
;; 	   ("If" exps "Then"
;; 		insts
;; 		"ElseIf" exps "Then"
;; 		insts
;; 		"ElseIf" exps "Then"
;; 		insts
;; 		"Else"
;; 		insts
;; 		"EndIf")

;; 	   ("While" exps "While-{" insts "WEnd")
;; 	   ("For" exp "To" exp "For-{" insts "Next")
;; 	   ("For" exp "To" exp "Step" exp "For-{" insts "Next")

;; 	   ("Switch" exps "Switch-{" SwitchBranches "EndSwitch")

;; 	   (exps))

;; 	  (SwitchBranches (SwitchBranches "Case-;" SwitchBranches)
;; 					  ("Case" exp "Case-{" insts)
;; 					  ;; ("Case" var "Case-{" exp)
;; 					  )

;; 	  (insts (insts "eol" insts) (inst))

;; 	  (exp (exp "+" exp)
;; 		   (exp "-" exp)
;; 		   (exp "*" exp)
;; 		   (exp "/" exp)
;; 		   (exp "=" exp)
;; 		   (exp "+=" exp)
;; 		   (exp "-=" exp)
;; 		   (exp "*=" exp)
;; 		   (exp "/=" exp)
;; 		   (exp "<" exp)
;; 		   (exp "<=" exp)
;; 		   (exp ">" exp)
;; 		   (exp ">=" exp)
;; 		   (exp "<>" exp)
;; 		   ("Global" exp)
;; 		   ("Local" exp)
;; 		   ("(" exps ")"))

;; 	  (exps (exps "," exps) (exp))
;; 	  (eols (eols "eol") ("eol"))
;; 	  )

;; 	'((assoc "eol"))
;; 	'((assoc ","))
;; 	'((assoc "<") (assoc ">") (assoc "<>") (assoc "<=") (assoc ">="))
;; 	'((assoc "+=") (assoc "-=") (assoc "*=") (assoc "/=") (assoc "="))
;; 	'((assoc "+") (assoc "-") (assoc "*") (assoc "/")))))

;; (defun autoit-smie-rules (kind token)
;;   "SMIE indentation rules for AutoIT language."
;;   (let ((ret
;; 		 (pcase (cons kind token)
;; 		   ;; (`(:after . "For") autoit-indent-offset)
;; 		   (`(:elem   . basic) autoit-indent-offset)
;; 		   (`(:elem   . empty-line-token) "")
;; 		   ;; (`(:elem   . args) (- (save-excursion (beginning-of-line-text) (point)) (point)))
;; 		   (`(:list-intro . ,_) t)
;; 		   ;; (`(,(or :close-all :after)  . ")") (- (save-excursion (beginning-of-line-text) (point)) (point)))
;; 		   (`(:before . "eol") (if (smie-rule-prev-p "Then")
;; 								   (progn
;; 									 autoit-indent-offset)
;; 								 nil))
;; 		   ;; (`(,(or :after :before) . "eol") 0)
;; 		   ;; (`(:after  . "Func") autoit-indent-offset)
;; 		   ;; (`(:before . "EndFunc") (smie-rule-parent))
;; 		   ;; (`(:after . ,(or "Else" "Then")) autoit-indent-offset)
;; 		   ;; (`(:after . ,(or "For" "While" "For-{" "While-{")) autoit-indent-offset)
;; 		   (`(:before . ,(or "Next" "WEnd" "EndIf")) (smie-rule-parent))
;; 		   )))
;; 	(message "[AAA] %S : %S = %S" kind token ret)
;; 	ret))

;; (defun autoit-smie-forward-token ()
;;   (let ((moved t)
;; 		tok)
;; 	(setq tok
;; 		  (cond
;; 		   ((looking-at-p "[ \t;]*\n")
;; 			(cond
;; 			 ((looking-back "For\\s-+\\w+\\s-*=\\s-*\\w+\\s-+To.*")
;; 			  (forward-line)
;; 			  (beginning-of-line)
;; 			  "For-{")
;; 			 ((looking-back "\\(Else\\)*If\\s-+.*Then")
;; 			  (forward-line)
;; 			  (beginning-of-line)
;; 			  "If-{")
;; 			 ((looking-back "Switch\\s-+.*")
;; 			  (forward-line)
;; 			  (beginning-of-line)
;; 			  "Switch-{")
;; 			 ((looking-back "Case\\s-+.*")
;; 			  (forward-line)
;; 			  (beginning-of-line)
;; 			  "Case-{")
;; 			 ((looking-back "While\\s-+.*")
;; 			  (forward-line)
;; 			  (beginning-of-line)
;; 			  "While-{")
;; 			 ((save-excursion
;; 				(forward-line)
;; 				(beginning-of-line)
;; 				(looking-at-p "^[ \t]*Case"))
;; 			  (forward-line)
;; 			  (beginning-of-line)
;; 			  "Case-;")
;; 			 (t
;; 			  (forward-line)
;; 			  (beginning-of-line)
;; 			  "eol")))
;; 		   (t
;; 			(setq moved nil))))
;; 	(if moved
;; 		tok
;; 	  (smie-default-forward-token))))

;; (defun autoit-smie-backward-token ()
;;   (let ((moved t)
;; 		(new-point (point))
;; 		tok)
;; 	(setq tok
;; 		  (cond
;; 		   ((looking-back "^[ \t]*")
;; 			(if (save-excursion
;; 				  (beginning-of-line)
;; 				  (looking-at "^[ \t]+Case"))
;; 				(progn
;; 				  (forward-line -1)
;; 				  (end-of-line)
;; 				  (setq new-point (point))
;; 				  "Case-;")
;; 			  (save-excursion
;; 				(forward-line -1)
;; 				(end-of-line)
;; 				(setq new-point (point))
;; 				(cond
;; 				 ((looking-back "For\\s-+\\w+\\s-*=\\s-*\\w+\\s-+To.*")
;; 				  "For-{")
;; 				 ((looking-back "\\(Else\\)*If\\s-+.*Then")
;; 				  "If-{")
;; 				 ((looking-back "Switch.*")
;; 				  "Switch-{")
;; 				 ((looking-back "While\\s-+.*")
;; 				  "While-{")
;; 				 (t
;; 				  "eol")))))
;; 		   ((looking-back "Case")
;; 			;;(goto-char (match-beginning 0))
;; 			(setq new-point (match-beginning 0))
;; 			"Case")
;; 		   ((looking-back "Case[ \t]+")
;; 			(skip-chars-backward " \t")
;; 			(setq new-point (point))
;; 			"Case-{")
;; 		   (t (setq moved nil))))
;; 	(if moved
;; 		(progn
;; 		  (when new-point
;; 			(goto-char new-point))
;; 		  tok)
;; 	  (smie-default-backward-token))))

;; (defun autoit-smie-setup ()
;;   "Set up SMIE for AutoIT mode."
;;   (smie-setup autoit-smie-grammar #'autoit-smie-rules
;;               :forward-token #'autoit-smie-forward-token
;;               :backward-token #'autoit-smie-backward-token))

;; (defun autoit-smie-forward-token-1 ()
;;   "SMIE forward token function for AutoIT mode."
;;   (let ((tok (smie-default-forward-token)))
;;     (cond ((eq tok :list-intro) (smie-rule-parent))
;;           ((eq tok :before) (smie-rule-parent))
;;           (t tok))))

;; (defun autoit-smie-backward-token-1 ()
;;   "SMIE backward token function for AutoIT mode."
;;   (let ((tok (smie-default-backward-token)))
;;     (cond ((eq tok :list-intro) (smie-rule-parent))
;;           ((eq tok :before) (smie-rule-parent))
;;           (t tok))))

;; (define-derived-mode autoit-mode prog-mode "AutoIT Mode"
;;   "Major mode for editing AutoIT code."
;;   (setq-local smie-indent-basic autoit-indent-offset)
;;   (autoit-smie-setup))


;; ) Attempt at SMIE END




;; Indentation BEGIN (

(defvar autoit-indent-regexps
  (list
   '(:if            . "^[ \t]*If\\s-+.+Then\\s-*\\(;+.*\\)?\\s-*$")
   '(:if-oneline    . "^[ \t]*If\\s-+.+Then\\s-+.*EndIf\\>")
   '(:if-branch     . "^[ \t]*\\(Else\\|ElseIf\\s-+.+Then\\)\\>")
   '(:if-end        . "^[ \t]*EndIf\\>")

   '(:select        . "^[ \t]*Select\\>")
   '(:select-end    . "^[ \t]*EndSelect\\>")
   '(:switch        . "^[ \t]*Switch\\s-+.+")
   '(:switch-end    . "^[ \t]*EndSwitch\\>")

   '(:case          . "^[ \t]*\\(Select\\>\\|Switch\\s-+.+\\)")
   '(:case-branch   . "^[ \t]*Case\\s-+.+")
   '(:case-end      . "^[ \t]*\\(EndSelect\\|EndSwitch\\)\\>")

   '(:for           . "^[ \t]*For\\s-+.+To\\s-+.+")
   '(:for-end       . "^[ \t]*Next\\>")
   '(:while         . "^[ \t]*While\\s-+.+")
   '(:while-end     . "^[ \t]*WEnd\\>")
   '(:func          . "^[ \t]*Func\\s-*\\(\\sw+\\)?\\s-*\\((.*)\\)\\s-*")
   '(:func-end      . "^[ \t]*EndFunc\\>")
   ;;'(:-skip         . "^[ \t]*\\([#;]+.*\\)?\\(If\\s-+.+Then\\s-+.+EndIf.*\\)?$")
   '(:-skip         . "^[ \t]*\\([#;]+.*\\)?$")
   ;; '(:-cont          . "^.*_[ \t]*\\(;+.*\\)[ \t]*$")
   ))

(defun autoit-indent-looking-at (&optional la-type)
  (beginning-of-line)
  (find (lambda (cs)
		  (and (looking-at-p (cdr cs))
			   cs))
		autoit-indent-regexps
		:test (lambda (fl cs)
				(let ((la-cons (funcall fl cs)))
				  (if la-type
					  (when (eq la-type (car la-cons))
						la-cons)
					la-cons)))))

(defun autoit-indent-looking-at-cont-p ()
  (end-of-line)
  (let ((inhibit-changing-match-data t))
	(prog1 (looking-back "_[ \t]*$")
	  (beginning-of-line))))

(defun autoit-indent-looking-at-skip ()
  (beginning-of-line)
  (find #'looking-at-p
		(mapcan
		 (lambda (it)
		   (when (string-suffix-p "-skip" (symbol-name (car it)))
			 (list it)))
		 autoit-indent-regexps)
		:test (lambda (fl cs)
				(funcall fl (cdr cs)))))

(defun autoit-indent-looking-at-oneline (&optional oneline-type)
  (beginning-of-line)
  (find (lambda (cs)
		  (and (looking-at-p (cdr cs))
			   cs))
		(mapcan
		 (lambda (it)
		   (when (string-suffix-p "-oneline" (symbol-name (car it)))
			 (list it)))
		 autoit-indent-regexps)
		:test (lambda (fl cs)
				(let ((oneline-cons (funcall fl cs)))
				  (if oneline-type
					  (when (eq oneline-type (car oneline-cons))
						oneline-cons)
					oneline-cons)))))

(defun autoit-indent-looking-at-start (&optional start-type)
  (beginning-of-line)
  (find (lambda (cs)
		  (and (looking-at-p (cdr cs))
			   cs))
		(mapcan
		 (lambda (it)
		   (unless (search "-" (symbol-name (car it)))
			 (list it)))
		 autoit-indent-regexps)
		:test (lambda (fl cs)
				(let ((start-cons (funcall fl cs)))
				  (if start-type
					  (when (eq start-type (car start-cons))
						start-cons)
					start-cons)))))

(defun autoit-indent-looking-at-branch (&optional branch-type)
  (beginning-of-line)
  (find (lambda (cs)
		  (and (looking-at-p (cdr cs))
			   cs))
		(mapcan
		 (lambda (it)
		   (when (string-suffix-p "-branch" (symbol-name (car it)))
			 (list it)))
		 autoit-indent-regexps)
		:test (lambda (fl cs)
				(let ((branch-cons (funcall fl cs)))
				  (if branch-type
					  (when (eq branch-type (car branch-cons))
						branch-cons)
					branch-cons)))))

(defun autoit-indent-looking-at-end (&optional end-type)
  (beginning-of-line)
  (find (lambda (cs)
		  (and (looking-at-p (cdr cs))
			   cs))
		(mapcan
		 (lambda (it)
		   (when (string-suffix-p "-end" (symbol-name (car it)))
			 (list it)))
		 autoit-indent-regexps)
		:test (lambda (fl cs)
				(let ((end-cons (funcall fl cs)))
				  (if end-type
					  (when (eq end-type (car end-cons))
						end-cons)
					end-cons)))))

(defun autoit-indent-skip-skip-backward ()
  (while (and (or (autoit-indent-looking-at-skip)
				  (nth 8 (syntax-ppss))     ; in string or comment
				  (< 0 (car (syntax-ppss))) ; in parens
				  )
			  (not (bobp)))
	(forward-line -1)))

(defun autoit-indent-goto-prev-line-skip-nested-blocks (block-type &optional end-kw stop-at-nested-start)
  (if block-type
	  (let ((do-search t)
			(nesting-level 0)
			(start-linum (line-number-at-pos))
			;; (endkw (or end-kw
			;; 		   (intern (concat (symbol-name block-type) "-end"))))
			end-start?)
		(while do-search
		  (cond
		   ((setq end-start? (autoit-indent-looking-at-oneline))
			(forward-line -1))
		   ((setq end-start? (autoit-indent-looking-at-end ;; endkw
														   ))
			(incf nesting-level)
			(forward-line -1))
		   ((setq end-start? (autoit-indent-looking-at-start ;; block-type
															 ))
			(when (> nesting-level 0)
			  (decf nesting-level))
			(forward-line -1)
			(when (and stop-at-nested-start
					   (< nesting-level 1))
			  (forward-line 1)
			  (when (= start-linum (line-number-at-pos))
				(forward-line -1))))
		   (t
			(forward-line -1)))
		  (when (< nesting-level 1)
			(setq do-search nil))
		  (autoit-indent-skip-skip-backward)
		  (when (bobp)
			(setq do-search nil))))
	(forward-line -1)
	(autoit-indent-skip-skip-backward)))

(defun autoit-indent-goto-prev-start (start-type)
  (let* ((tmp-sym-name (symbol-name start-type))
		 (block-type (intern (substring tmp-sym-name 0 (search "-" tmp-sym-name))))
		 (do-search t)
		 ltype)
	(while do-search
	  (autoit-indent-goto-prev-line-skip-nested-blocks block-type nil t)
	  (when (bobp)
		(setq do-search nil))
	  (cond
	   ((setq ltype (autoit-indent-looking-at-start block-type))
		(setq do-search nil))))))

(defun autoit-indent-goto-prev-branch-or-start (branch-type)
  (let* ((tmp-sym-name (symbol-name branch-type))
		 (block-type (intern (substring tmp-sym-name 0 (search "-" tmp-sym-name))))
		 (br-type (intern (concat (symbol-name block-type) "-branch")))
		 (do-search t)
		 ltype)
	(while do-search
	  (autoit-indent-goto-prev-line-skip-nested-blocks block-type)
	  (when (bobp)
		(setq do-search nil))
	  (cond
	   ((setq ltype (autoit-indent-looking-at-branch branch-type))
		(setq do-search nil))
	   ((setq ltype (autoit-indent-looking-at-start block-type))
		(setq do-search nil))))))

(defun autoit-indent-goto-prev-indented-line ()
  (let (lat)
	(cond
	 ((setq lat (autoit-indent-looking-at-oneline))
	  (forward-line -1)
	  (autoit-indent-skip-skip-backward)
	  lat)
	 ((setq lat (autoit-indent-looking-at-branch))
	  (autoit-indent-goto-prev-branch-or-start (car lat))
	  lat)
	 ((setq lat (autoit-indent-looking-at-end))
	  (autoit-indent-goto-prev-start (car lat))
	  lat)
	 ((setq lat (autoit-indent-looking-at-start))
	  (forward-line -1)
	  (autoit-indent-skip-skip-backward)
	  lat)
	 (t
	  (forward-line -1)
	  (unless (autoit-indent-looking-at-cont-p)
		(autoit-indent-skip-skip-backward))
	  nil))))


(defun autoit-indent-line ()
  "Indent current line in `autoit-mode'."
  (interactive)
  (let* ((lil nil)
		 (lil-cont nil)
		 (lil-indent (current-indentation))
		 (lil-num (line-number-at-pos))
		 (cul (autoit-indent-looking-at))
		 (clinum (line-number-at-pos)))
	(save-excursion
	  (autoit-indent-goto-prev-indented-line)
	  (setq lil (autoit-indent-looking-at))
	  (setq lil-cont (autoit-indent-looking-at-cont-p))
	  (setq lil-indent (current-indentation))
	  (setq lil-num (line-number-at-pos)))
	(when (< 1 (- clinum lil-num))
	  (setq lil-cont nil))
	(message "%S %S %S" lil-cont lil-indent (cons (car lil) (car cul)))
	(indent-line-to
	 (pcase (cons (car lil) (car cul))
	   (`(:if . ,(or :if-branch :if-end))
		lil-indent)
	   (`(:if . ,_)
		(+ lil-indent tab-width))
	   (`(:if-branch . :if-branch)
		lil-indent)
	   (`(:if-branch . ,_)
		(+ lil-indent tab-width))
	   (`(:if-end . ,_)
		lil-indent)

	   (`(:switch . ,(or :case-branch :switch-end))
		lil-indent)
	   (`(:switch . ,_)
		(+ lil-indent tab-width))
	   (`(:switch-end . ,_)
		lil-indent)

	   (`(:select . ,(or :case-branch :select-end))
		lil-indent)
	   (`(:select . ,_)
		(+ lil-indent tab-width))
	   (`(:select-end . ,_)
		lil-indent)

	   ;; (`(:switch . :case-branch)
	   ;; 	(+ lil-indent tab-width))
	   ;; (`(:select . :case-branch)
	   ;; 	(+ lil-indent tab-width))

	   (`(:case-branch . :case-branch)
		lil-indent)
	   (`(:case-branch . ,_)
		(+ lil-indent tab-width))

	   (`(:func . :func-end)
		lil-indent)
	   (`(:func . ,_)
		(+ lil-indent tab-width))
	   (`(:func-end . ,_)
		lil-indent)

	   (`(:while . :while-end)
		lil-indent)
	   (`(:while . ,_)
		(+ lil-indent tab-width))
	   (`(:while-end . ,_)
		lil-indent)

	   (`(:for . :for-end)
		lil-indent)
	   (`(:for . ,_)
		(+ lil-indent tab-width))
	   (`(:for-end . ,_)
		lil-indent)

	   ;; (`(nil . ,(or :if :if-branch :switch :select :case-branch :for :while :func))
	   ;; 	(+ lil-indent tab-width))

	   ;; (`(,_ . :-skip)
	   ;; 	0)

	   (_
		(if lil-cont
			(+ lil-indent tab-width)
		  lil-indent))))))

;; ) Indentation END




;; Mode definition BEGIN (

(defvar autoit-mode-map
  (let ((map (make-keymap)))
    ;;(define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for AutoIT major mode")


;;;###autoload
(define-derived-mode autoit-mode prog-mode "AutoIT"
  "Major mode for editing AutoIT scripts"

  (setq font-lock-defaults '(autoit-font-lock-keywords))

  ;; (set (make-local-variable 'comment-start) ";")
  ;; (set (make-local-variable 'comment-end)   "")
  ;; (set (make-local-variable 'comment-use-syntax) t)

  (setq-local indent-line-function #'autoit-indent-line)
  ;; (set-syntax-table autoit-mode-syntax-table)
  ;; (use-local-map autoit-mode-map)
  )


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.au3\\'" . autoit-mode))


(provide 'autoit-mode)

;; ) Mode definition END



;; Local Variables:
;; indent-tabs-mode: t
;; End:


;;; autoit-mode.el ends here
