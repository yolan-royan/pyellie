;;; pyellie.el - A basic variable explorer for emacs 

;; Copyright (C) 2019 Yolan Royan

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


(require 'ivy)
(require 'popup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp wrappers to the python interpreter ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'python-eval 'python-shell-send-string-no-output)

(defun tokenize (python-list)
  "Convert PYTHON-LIST expressed in string format to an elisp
slist of strings"
  (split-string
   ; we first remove the square brackets, extra whitepace and string
   ; markers, then split by comma
   (replace-regexp-in-string "[][' ]" "" python-list)
   ","))

(defun eval-python-function (python-function argument)
  "Evaluate PYTHON-FUNCTION that takes single argument ARGUMENT and return the result as a string" 
  (condition-case nil 
  (let* ((python-command (concat python-function "(" argument ")")))
    (python-eval python-command))
  (error "could not contact the python shell")))

(defun python-dir (python-variable)
  (eval-python-function "dir" python-variable))

(defun python-print (python-variable)
  (eval-python-function "print" python-variable))

(defun python-clean-type (raw-python-type-string)
"The raw output of the python type() function is <class '{datatype}'>. This function removes the fluff at the beginning and end so we're just left with {datatype}"
  (let* ((string-length (length raw-python-type-string))
       (end (- string-length 2))
       (start 8))
  (substring raw-python-type-string start end)))

(defun python-type (python-variable)
   (python-clean-type (eval-python-function "type" python-variable)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy-rich transformer binding ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq original-transformers-list ivy-rich--display-transformers-list)
;(setq ivy-rich--display-transformers-list original-transformers-list)


(setq ivy-rich--display-transformers-list
(append ivy-rich--display-transformers-list
	'(python-list-variables
	  (:columns ((ivy-rich-candidate (:width 40))
		     (python-type (:width 40)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pandas dataframe API ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval-python-to-buffer (python-command &optional buffer-name)
  (unless buffer-name (setq buffer-name "*foo*"))
  (save-excursion
    (pop-to-buffer buffer-name)
    (if buffer-read-only (read-only-mode 0)) 
    (erase-buffer)
    (insert (python-eval python-command))
    (special-mode)))


(defun pandas-is-dataframe-p (python-variable)
  "asks the python interpreter if python-variable is a dataframe"
  (equal (python-type python-variable) "pandas.core.frame.DataFrame"))


(defun pandas-head (df &optional num-rows)
  (unless num-rows (setq num-rows 10))
  (unless (stringp num-rows) (setq num-rows (number-to-string num-rows)))
  (let ((python-command (concat df ".head(" num-rows ")")))
	(eval-python-to-buffer python-command)))

(defun pandas-describe (df)
  (let ((python-command (concat df ".describe()")))
	(eval-python-to-buffer python-command)))

(defun pandas-info (df)
   (let ((python-command (concat df ".info()")))
    (eval-python-to-buffer python-command)))

(defun pandas-first-row (df)
   (let ((python-command (concat df ".head(1).T")))
    (eval-python-to-buffer python-command)))

(defun pandas-get-columns (df)
  (tokenize (python-eval (concat df ".columns.tolist()"))))

(defun pandas-get-numeric-columns (df)
  (tokenize (python-eval (concat df ".select_dtypes(include='number').columns.tolist()"))))

(defun pandas-summarize-variable-helper (df var)
  (save-excursion
    (pop-to-buffer "*foo*")
    (if buffer-read-only (read-only-mode 0)) 
    (erase-buffer)
    ;; had an annoying issue where org-mode init returns an error. This should fix it
    (ignore-errors (org-mode))
    (insert (concat "* Variable " var "\n"))
    (insert (concat "\n* Type? \n"))
    (insert (python-eval (concat df "." var ".dtype")))
    (setq yolo-is-numeric (member var (pandas-get-numeric-columns df)))
    (insert (concat "\n* Cardinality\n")) 
    (insert (python-eval (concat df "." var ".nunique()")))
    (insert (concat "\n* Value Counts\n")) 
    (insert (python-eval (concat df "." var ".value_counts()")))
    (if yolo-is-numeric
	(progn
	(insert (concat "\n* Quantiles\n"))
	(insert (python-eval (concat df "." var ".quantile(np.arange(0,1,0.1))")))))
    (insert (concat "\n* Missingness\n")) 
    (insert (python-eval (concat df "." var ".isnull().sum()")))))


(defun pandas-summarize-variable-in-dataframe (df)
  (ivy-read "Column:"(pandas-get-columns df) :action (lambda (v) (pandas-summarize-variable-helper df v))))


;; recursive dirrer

(defun python-recursive-dir (&optional var)
  (interactive)
  (unless var (setq var ""))
  (ivy-read "dir:" (mapcar (lambda (x) (concat var (if (equal var "") "" ".") x)) (tokenize (python-dir var))) :action (lambda (x) (python-recursive-dir x))))


(setq ivy-rich--display-transformers-list
(append ivy-rich--display-transformers-list
	'(python-recursive-dir
	  (:columns ((ivy-rich-candidate (:width 50))
		     (python-type (:width 40)))))))


;(recursive-dir "")

;; Hydras

(defun make-pandas-hydra (df)
    (eval (replace-regexp-in-string "df" df
"
  (defhydra pandas-hydra ()

     (concat \"Pandas DataFrame: \" \"df\"
	     \"
_h_ : head 
_f_ : first row 
_i_ : info 
_d_ : describe
_s_ : summarize variable 
\")
    
    (\"h\" (pandas-head \"df\") :exit t)
    (\"f\" (pandas-first-row \"df\") :exit t)
    (\"i\" (pandas-info \"df\") :exit t)
    (\"d\" (pandas-describe \"df\") :exit t)
    (\"s\" (pandas-summarize-variable-in-dataframe \"df\") :exit t))")))




;;variable functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun python-not-module-p (python-variable)
 (not (equal (python-type python-variable) "module")))

(defun python-not-special-variable-p (var)
  "Checks if VAR starts with __ (python usually denotes special
variables like this and we usually don't want to see them"
  (not (string-match "^__" var)))

(defun variable-filter (python-variable)
  (and (python-not-special-variable-p python-variable)
       (python-not-module-p python-variable)))

(defun describe-var (var)
  (save-excursion
    (pop-to-buffer "*foo*")
    (if buffer-read-only (read-only-mode 0))
    (erase-buffer)
    (insert (python-type var))
    (newline 2)
    (insert (python-print var))
    (special-mode)))

;; adapted from https://emacs.stackexchange.com/questions/19877/how-to-evaluate-elisp-code-contained-in-a-string-
(defun my-eval-string (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun python-list-dispatcher (var)
  (if (pandas-is-dataframe-p var)
      (progn (my-eval-string (make-pandas-hydra var)) (pandas-hydra/body))
      (describe-var var)))


(defun python-list-variables ()
  (interactive)
  (ivy-read "Var:" (seq-filter 'variable-filter (tokenize (python-dir "")))
	    :action (lambda (x) (describe-var x))
	    :preselect (ivy-thing-at-point)))


(ivy-set-actions
 'python-list-variables
 '(("p" describe-var "print this variable")
   ("d" python-list-dispatcher "cool stuff")))

(ivy-set-actions
 'python-recursive-dir
 '(("p" describe-var "print this variable")
   ("d" python-list-dispatcher "cool stuff")))


(defun python-query-at-point ()
  (interactive)
  (let ((var (ivy-thing-at-point)))
    (python-list-dispatcher  var)))



(defhydra pandas-hydra ()

     (concat "Pandas DataFrame: " "df"
	     "
	     _h_ : head 
	     _f_ : first row 
	     _i_ : info 
	     _d_ : describe
	     _s_ : summarize variable 
	     ")
    
    ("h" (pandas-head "df") :exit t)
    ("f" (pandas-first-row "df") :exit t)
    ("i" (pandas-info "df") :exit t)
    ("d" (pandas-describe "df") :exit t)
    ("s" (pandas-summarize-variable-in-dataframe "df") :exit t))



;;;;;;;;;;;;;;;;;;;;;
;; popup interface ;;
;;;;;;;;;;;;;;;;;;;;;

(defun eval-python-to-popup (python-command)
    (popup-tip (python-eval python-command)))

(defun pandas-head-popup (df &optional num-rows)
  (unless num-rows (setq num-rows 10))
  (unless (stringp num-rows) (setq num-rows (number-to-string num-rows)))
  (let ((python-command (concat df ".head(" num-rows ")")))
	(eval-python-to-popup python-command)))

(defun describe-var-popup (var)
  (let* ((var-type (python-eval (concat "type(" var ")")))
	 (var-value (python-print var))
	 (popup-text (concat "Variable " var "\n" var-type "\nCurrent Value\n\n" var-value)))
    (popup-tip popup-text)))

(defun python-list-dispatcher-popup (var)
  (if (pandas-is-dataframe-p var)
      (pandas-head-popup var)
      (describe-var-popup var)))

(defun python-query-at-point-popup ()
  (interactive)
  (let ((var (ivy-thing-at-point)))
    (python-list-dispatcher-popup var)))

(define-minor-mode pyellie-mode
  :lighter " pyellie")

(provide 'pyellie-mode)

