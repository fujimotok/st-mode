;;; st-mode.el --- A mode for Programing Lanuguages of IEC61131-3
;;;
;;;
;;;
;;;
;;; Commentary:
;;;

;;; Code:

(require 'rx)

;;; defvars --------------------------------------------------------------------

;;;
;;; mode-map
;;;
(defvar st-mode-map nil
  "Key map for `st-mode`")
(setq st-mode-map
      (let ((map (make-keymap)))
	(define-key map "\C-j" 'newline-and-indent)
	map))

;;;
;;; indent-regex
;;;
(defun iec61131-regex-or ()
  ""
  "\\|")

(defun iec61131-regex-endswith (seq)
  ""
  (let ((words (regexp-opt seq)))
    (concat ".*" words ".*$")))

(defun iec61131-regex-startswith (seq)
  ""
  (let ((words (regexp-opt seq)))
    (concat "[ \t]*" words)))

(defun iec61131-regex-case-label ()
  ""
  "^.*:[^;]*$")

(defvar iec61131-indent-regex+1 nil "Regex for indentation increment.")
(setq iec61131-indent-regex+1
      (concat
       (iec61131-regex-endswith '("THEN" "DO" "ELSE" "(" "(*" "{" "["))
       (iec61131-regex-or)
       (iec61131-regex-startswith '("VAR"))
       (iec61131-regex-or)
       (iec61131-regex-case-label)))

(defvar iec61131-indent-regex-1 nil "Regex for indentation decrement.")
(setq iec61131-indent-regex-1
      (concat
       (iec61131-regex-startswith '("END_" "ELSE" ")" "*)" "}" "]"))
       (iec61131-regex-or)
       (iec61131-regex-case-label)))

;;;
;;; font-lock-keywords
;;;
(defvar iec61131-keywords nil "Keywords to be highlighted.")
(setq iec61131-keywords
      (list "ANY" "ANY_BIT" "ANY_DATE" "ANY_DERIVED" "VAR_OUTPUT"
	    "ANY_ELEMENTARY" "ANY_INT" "ANY_NUM" "ANY_REAL"
	    "ANY_STRING" "ARRAY" "BOOL"
	    "BYTE" "DATE_AND_TIME" "DINT" "DWORD"
	    "INT" "LINT"
	    "LREAL" "LWORD" "REAL" "SINT" "STRING"
	    "TIME" "TIME_OF_DAY" "TOD" "UDINT" "UINT"
	    "ULINT" "USINT" "WORD" "WSTRING" "AT"
	    "BY" "CASE" "COLON" "CONFIGURATION"
	    "CONSTANT" "DATE" "DO" "DT"
	    "ELSE" "ELSEIF" "END_CASE" "END_CONFIGURATION"
	    "END_FOR" "END_FUNCTION" "END_FUNCTION_BLOCK"
	    "END_IF" "END_PROGRAM" "END_REPEAT" "END_RESOURCE"
	    "END_STRUCT" "END_TYPE" "END_VAR" "END_WHILE"
	    "EXIT" "FOR" "FUNCTION" "FUNCTION_BLOCK"
	    "F_EDGE" "IF" "INTERVAL" "NIL"
	    "NON_RETAIN" "OF" "ON" "PRIORITY" "PROGRAM"
	    "READ_ONLY" "READ_WRITE" "REPEAT" "RESOURCE"
	    "RETAIN" "RETURN" "RIGHT_ARROW" "R_EDGE"
	    "SINGLE" "STRUCT" "TASK" "THEN" "TO"
	    "TYPE" "UNTIL" "VAR" "VAR_ACCESS" "VAR_CONFIG"
	    "VAR_EXTERNAL" "VAR_GLOBAL" "VAR_INPUT" "VAR_IN_OUT"
	    "VAR_TEMP" "WHILE" "WITH"))

(defvar iec61131-single-line-comment-regex nil
  "Regex for single-line comments //.")
(setq iec61131-single-line-comment-regex
      (rx (group "//" (?? not-newline) "\n")))

(defvar iec61131-multi-line-comment-regex nil
  "Regex for multi-line comments.")
(setq iec61131-multi-line-comment-regex
      (rx (or (group "/*" (?? anything) "*/")
	      (group "(*" (?? anything) "*)"))))

(defvar iec61131-string-regex nil
  "Regex for string literals.")
(setq iec61131-string-regex
      (rx (or (group "\"" (?? anything) "\"")
	      (group ?' (?? anything) ?'))))

(defvar iec61131-time-regex
  nil "\\(TIME\\|T\\)[#]\\([0-9_]+\\(\\.[0-9]+\\)?\\(ms\\|s\\|m\\|h\\|d\\)\\)+.")
(setq iec61131-time-regex
      (rx word-start (or "TIME" "T") "#" (group (one-or-more (one-or-more digit)
					                     (opt ?. (one-or-more digit))
					                     (or "ms" ?s ?h ?m ?d))) word-end))

(defvar iec61131-date-regex nil "")
(setq iec61131-date-regex
      (rx word-start (or "DATE" "D") "#"
	  (group (repeat 4 digit) ?- (repeat 2 digit) ?- (repeat 2 digit)) word-end))

(defvar iec61131-datetime-regex nil)
(setq iec61131-datetime-regex
      (rx word-start (or "DATE_AND_TIME" "DT") ?#
	  (group (repeat 4 digit) ?- (repeat 2 digit) ?- (repeat 2 digit))
	  ?: (repeat 2 digit) ?: (repeat 2 digit) ?: (repeat 2 digit) word-end))

(defvar iec61131-bool-regex nil)
(setq iec61131-bool-regex (rx word-start (or "TRUE" "FALSE" "BIT#1" "BIT#0") word-end))

(defvar iec61131-font-lock-keywords "" nil)
(setq iec61131-font-lock-keywords
      `(
        (,iec61131-multi-line-comment-regex . font-lock-comment-face)
        (,iec61131-string-regex . font-lock-constant-face)
        (,iec61131-time-regex . font-lock-constant-face)
        (,iec61131-datetime-regex . font-lock-constant-face)
        (,iec61131-date-regex . font-lock-constant-face)
        ("\\(TIME_OF_DAY\\|TOD\\)#[012][0-9]:[0-5][0-9]:[0-5][0-9]\\(\\.[0-9]\\{,3\\}\\)"
         . font-lock-constant-face)
        ("\\<.*#.*\\>" . font-lock-constant-face)
        ("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
        (,iec61131-bool-regex  . font-lock-constant-face)
        (,(concat "\\<" (regexp-opt iec61131-keywords) "\\>") . font-lock-builtin-face)))

;;;
;;; syntax-teble
;;;
(defvar st-mode-syntax-table nil
  "IEC 61131 Syntax Table.")
(setq st-mode-syntax-table
      (let ((table (make-syntax-table)))
	(modify-syntax-entry ?_ "w" table)
	(modify-syntax-entry ?/ ". 12b" table)
	(modify-syntax-entry 40 ". 1n" table) ;; (
	(modify-syntax-entry 41 ". 4n" table) ;; )
	(modify-syntax-entry ?* ". 23n" table)
	(modify-syntax-entry ?\n "> b" table)
	table))


;;; defuns ---------------------------------------------------------------------

(defun iec61131-indent-line ()
  "Identation function for st-mode."
  (interactive)
  (beginning-of-line)
  (let ((not-indented t)
	cur-indent)
    (setq cur-indent
	  (save-excursion
	    (cond
	     ;; Beginning of the buffer => no indentation
	     ((bobp) 0)
	     ;; current line is deindentation
	     ((looking-at-p iec61131-indent-regex-1)
	      (forward-line -1) ;; do not understand this
	      (max 0
                   (- (current-indentation) tab-width)))
	     (t
	      (forward-line -1)
	      (if (looking-at-p iec61131-indent-regex+1)
		  (+ (current-indentation) tab-width)
		(current-indentation))))))
    (indent-line-to cur-indent)))

;;;
;;; major-mode-provide
;;;

;;;###autoload
(define-derived-mode st-mode fundamental-mode "ST"
  "A major mode for editing Structured Text files after IEC 61131-3"
  :syntax-table st-mode-syntax-table
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local font-lock-defaults
              '(iec61131-font-lock-keywords nil t)) ;set CASE-FOLD t
  (setq-local indent-line-function 'iec61131-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.st\\'" . st-mode))

(provide 'st-mode)

;;; st-mode.el ends here
