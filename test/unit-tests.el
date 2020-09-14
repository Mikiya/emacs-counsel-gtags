;;; unit-tests.el --- Unit tests for counsel-gtags   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Felipe Lema

;; Author: Felipe Lema <1232306+FelipeLema@users.noreply.github.com>
;; Keywords:

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
;;; See http://sachachua.com/blog/2015/02/continuous-integration-code-coverage-emacs-packages-travis-coveralls/
;;; See https://www.atlassian.com/continuous-delivery/continuous-integration-tutorial
;;; Because of an extra `find-file' and me running this interactively, you'll
;;; see a lot of (save-window-excursion …)


;;

;;; Code:

;; See https://emacs.stackexchange.com/a/19458/10785
(require 'cl-lib)
(require 'f)
(require 'dash)
(require 's)

(require 'counsel-gtags)

;;;;;;;;;;;;;;;;;
;; Infrastructure
;;;;;;;;;;;;;;;;;
(defvar counsel-gtags--expr nil
  "Holds a test expression to evaluate with `counsel-gtags--eval'.

Taken from ivy-tests.el")

(defvar counsel-gtags--result nil
  "Holds the eval result of `counsel-gtags--expr' by `ivy-eval'.

Taken from ivy-tests.el")

(defun counsel-gtags--eval ()
  "Evaluate `counsel-gtags--expr'.

Taken from ivy-tests.el"
  (interactive)
  (setq counsel-gtags--result (eval counsel-gtags--expr)))

(global-set-key (kbd "C-c e") 'counsel-gtags--eval)

(defun counsel-gtags--with (expr keys)
  "Evaluate EXPR followed by KEYS.

Taken from ivy-tests.el"
  (let ((counsel-gtags--expr expr))
    (execute-kbd-macro
     (vconcat (kbd "C-c e")
              (kbd keys)))
    counsel-gtags--result))

(defmacro counsel-gtags--with-mock-project (&rest body)
  "Create mock project with source file and execute BODY.

You can access the project at `default-directory', non-empty source file and
header at `main-file-path' and `main-header-path'."
  `(let* ((project-path (make-temp-file
			 "counsel-gtags-unit-test-mock-project"
			 t))
	  (default-directory (file-name-as-directory project-path))
	  (main-file-path (concat
			   (file-name-as-directory
			    default-directory)
			   "main.c"))
	  (main-header-path (concat
			     (file-name-as-directory
			      default-directory)
			     "main.h"))
	  (main-header-text "char *header_string;
int header_int;
float header_float;
void global_fun();
void another_global_fun();
")
	  (main-file-text  "
#include \"main.h\"

char *global_string;
int global_int;
float global_float;
void global_fun(){
    write(header_string);
    write(header_header_int);
}
void another_global_fun(){
    write(header_header_int);
}
int the_first_func() {
  return 0;
}

int the_second_func(int x) {
  return 0;
}

int the_third_func(int x) {
  return 0;
}
int main{
    int local_int;
}")
	  )
     (f-write-text main-file-text 'utf-8 main-file-path)
     (f-write-text main-header-text 'utf-8 main-header-path)
     (call-process "gtags")
     (save-window-excursion
       (let ((__result (progn
			 ,@body)))
	 ;; clean up buffers that may have been opened in the process before
	 ;; returning
	 (let* ((buffers-&-paths (mapcar
				  (lambda (buffer)
				    (list buffer
					  (with-current-buffer buffer
					    (and buffer-file-name
						 (file-name-directory buffer-file-name)))))
				  (buffer-list)))
		(buffers-from-project (remove-if-not
				       (lambda (buffer-&-path)
					 (and (stringp (cadr buffer-&-path))
					      (string-equal (cadr buffer-&-path)
							    project-path)))
				       buffers-&-paths)))
	   (dolist (buffer-&-path buffers-from-project)
	     (kill-buffer (cadr buffer-&-path))))
	 __result))))

(ert-deftest 00-can-create-project ()
  (counsel-gtags--with-mock-project
   (should (stringp (f-read main-file-path 'utf-8)))))

(ert-deftest 00-can-create-db-and-read-symbols ()
  "For now, we can only create tags interactively"
  (counsel-gtags--with-mock-project
   (should
    (string-equal (counsel-gtags--default-directory)
		  (file-name-as-directory default-directory)))))
(ert-deftest 00-gtags-command-works ()
  (counsel-gtags--with-mock-project
   (let* ((from-cmd (shell-command-to-string "global --result=grep another_global_fun"))
	  (clean (s-trim from-cmd)))
     (should (string-equal
	      "main.c:11:void another_global_fun(){"
	      clean)))))

(ert-deftest 00-can-query ()
  "For now, we can only create tags interactively"
  (counsel-gtags--with-mock-project
   (should
    (string-equal (counsel-gtags--default-directory)
		  (file-name-as-directory default-directory)))))

(ert-deftest 00-environment-have-grep-command ()
  "Assert that we have a grep tool of any kind"
  (should (counsel-gtags--get-grep-command)))

;;;;;;;;;;;;;;;;;
;; Actual testing
;;;;;;;;;;;;;;;;;
(ert-deftest correct-collection-of-candidates ()
  (counsel-gtags--with-mock-project
   (let* ((counsel-gtags-path-style 'root)
	  (root (counsel-gtags--default-directory))
          (default-directory root)
	  (type 'definition)
	  (tagname "another_global_fun")
          (encoding buffer-file-coding-system)
	  (extra-options)
	  (expected '("./main.c:11:void another_global_fun(){"))
	  (collected (counsel-gtags--collect-candidates type tagname encoding extra-options)))
     (should
      (equal collected expected)))))

(ert-deftest default-case-sensitive ()
  (let ((ivy-auto-select-single-candidate t)
	(counsel-gtags-path-style 'relative))
    (counsel-gtags--with-mock-project
     (should
      (string-prefix-p "main.c"
		       (counsel-gtags-find-definition
			"another_global_fun"))))))

(ert-deftest ignore-case ()
  (let ((counsel-gtags-path-style 'relative)
	(ivy-auto-select-single-candidate t)
	(counsel-gtags-ignore-case t))
    (counsel-gtags--with-mock-project
     (should
      (string-prefix-p "main.c"
		       (counsel-gtags-find-definition
			"ANOTHER_GLOBAL_FUN"))))))

(ert-deftest file-path-resolution ()
  "`counsel-gtags--remote-truename' resolves local file paths correctly.

No queries to global involved."
  (let* ((repo-root-path (locate-dominating-file "./" "counsel-gtags.el"))
	 (sample-project-path (concat (file-name-as-directory repo-root-path)
				      "test/sample-project"))
	 (expected-file-path (expand-file-name
			      (concat (file-name-as-directory sample-project-path)
				      "some-module/marichiweu.c")))
	 (default-directory (file-name-as-directory sample-project-path))
	 (resolved-file-path (counsel-gtags--remote-truename "some-module/marichiweu.c")))
    (should (string-equal
	     expected-file-path resolved-file-path))))

(ert-deftest file-path-results ()
  "Handling of results for file queries (global … -P …).

All file candidates"
  (let* ((counsel-gtags-path-style 'root)
	 (repo-root-path (locate-dominating-file "./" "counsel-gtags.el"))
	 (sample-project-path (concat (file-name-as-directory repo-root-path)
				      "test/sample-project"))
	 ;; use other-module as current directory
	 (default-directory (file-name-as-directory
			     (concat (file-name-as-directory sample-project-path)
				     "other-module")))
         (encoding (or buffer-file-coding-system
		       "utf-8-unix")))
    (should encoding)
    (let* ((candidates (counsel-gtags--get-files))
	   (actual-path-per-candidate (--map (counsel-gtags--resolve-actual-file-from it)
					     candidates))
	   (each-candidate-exist (-map #'file-exists-p actual-path-per-candidate)))
      (should
       (--all? it
	       each-candidate-exist)))))

(setq counsel-gtags--test-find-file-result nil)
(defun counsel-gtags--intercept-find-file (original-fun &rest args)
  "Intercept `find-file' & save its argument to special variable.

Special variable is `counsel-gtags--test-find-file-result'.
ORIGINAL-FUN is `find-file'; rest of arguments (ARGS) is the file."
  (setq counsel-gtags--test-find-file-result (-first-item args)))

(ert-deftest relative-to-project-root ()
  "Open files correctly when relative to root."
  (save-window-excursion
    (let* ( ;; locate paths
	   (repo-root-path (locate-dominating-file "./" "counsel-gtags.el"))
	   (sample-project-path (concat (file-name-as-directory repo-root-path)
					"test/sample-project/"))
	   ;; configure test
	   (counsel-gtags-path-style 'root)
	   (default-directory sample-project-path)
	   (expected
	    (split-string (shell-command-to-string
			   ;;    ↓ root of files is expected to be ⎡.⎦
			   "find . -name '*.c' -or -name '*.h'")
			  "\n" t)))
      (let ((candidates
	     (-second-item
	      (counsel-gtags--find-file-ivy-parameters nil))))
	(should (equal
		 candidates
		 (-sort #'s-less? expected)))))))

(ert-deftest test-read-tag ()
  "Test `counsel-gtags--read-tag'.

Ivy documentation mentions that any call to `counsel--async-command' can be
tested with a call to `shell-command-to-string' and `split-string' like

(split-string (shell-command-to-string …)).

"
  (let* ((repo-root-path (locate-dominating-file "./" "counsel-gtags.el"))
	 (sample-project-path (concat (file-name-as-directory repo-root-path)
       				      "test/sample-project/")))
    (save-window-excursion
      (with-current-buffer (find-file (format "%s/main.c" sample-project-path))
	(goto-char (point-min))
	(search-forward "marichiweu"
			(point-max))

	(let* ((type 'symbol) ;; from `counsel-gtags-find-symbol'
	       (params (counsel-gtags--read-tag-ivy-parameters type))
	       (query (plist-get params :initial-input ))
	       (raw-string
		;; see `counsel-gtags--async-tag-query-process'
		(shell-command-to-string
		 (counsel-gtags--build-command-to-collect-candidates
		  query
		  '("--result=ctags")))))
	  (should
	   (string-equal "marichiweu\n" ;; single line
			 (replace-regexp-in-string (rx (* (char space)) line-end)
						   ""
						   raw-string))))))))



(ert-deftest using-tramp ()
  "See https://github.com/FelipeLema/emacs-counsel-gtags/issues/1"
  (counsel-gtags--with-mock-project
   (let ((current-dir default-directory))
     (cd "/") ;; ensure we're not in the project directory in the local machine
     (unwind-protect;; https://curiousprogrammer.wordpress.com/2009/06/08/error-handling-in-emacs-lisp/
	 (let ((remote-buffer
		(find-file (format "/ssh:localhost:%s"
			      (file-truename main-file-path)))))
	   ;; I expect `find-file' returns the new buffer
	   (should (bufferp remote-buffer))
	   (with-current-buffer remote-buffer
	     (let* ((root (counsel-gtags--default-directory)))
	       (should (file-remote-p root))
	       (let* ((default-directory root)
		      (type 'definition)
		      ;; ↓ from correct-collection-of-candidates
		      (tagname "another_global_fun")
		      (encoding buffer-file-coding-system)
		      (extra-options)
		      (expected '("./main.c:11:void another_global_fun(){"))
		      (collected
		       (counsel-gtags--collect-candidates type
							  tagname
							  encoding
							  extra-options)))
		 (should (equal collected expected))))))
       ;; restore previous dir
       (cd current-dir)))))

(ert-deftest select-file ()
  "See https://github.com/FelipeLema/emacs-counsel-gtags/issues/2"
  (counsel-gtags--with-mock-project
   (save-window-excursion
     (let ((default-directory (counsel-gtags--default-directory)))
       (cl-multiple-value-bind (the-buffer  the-line)
	   (counsel-gtags--jump-to "./main.c:11:void another_global_fun(){")
	 (should (string-equal
		  (file-name-nondirectory (buffer-file-name the-buffer))
		  "main.c"))
	 (should (equal the-line 11)))))))

(ert-deftest select-remote-file ()
  "See https://github.com/FelipeLema/emacs-counsel-gtags/issues/1#issuecomment-481333499"
  (counsel-gtags--with-mock-project
   (let ((current-dir default-directory))
     (cd "/") ;; ensure we're not in the project directory in the local machine
     (unwind-protect;; https://curiousprogrammer.wordpress.com/2009/06/08/error-handling-in-emacs-lisp/
	 (let ((remote-buffer
		(find-file (format "/ssh:localhost:%s"
			      (file-truename main-file-path)))))
	   (save-window-excursion
	     (with-current-buffer remote-buffer
	       (let* ((root (counsel-gtags--default-directory)))
		 (should (file-remote-p root))
		 (let* ((default-directory root)
			(type 'definition)
			;; ↓ from `correct-collection-of-candidates' test
			(tagname "another_global_fun")
			(extra-options)
			(auto-select-single-candidate t)
			(collection (-second-item
		      		     (counsel-gtags--select-file-ivy-parameters
				      type tagname extra-options auto-select-single-candidate))))
		   (should
		    (= (length collection) 1))
		   (cl-multiple-value-bind (the-buffer the-line)
		       (counsel-gtags--jump-to (car collection))
		     (should
		      (file-remote-p (buffer-file-name the-buffer)))))))))))))

(ert-deftest select-candidate ()
  "See https://github.com/FelipeLema/emacs-counsel-gtags/issues/8"
  (counsel-gtags--with-mock-project
   (save-window-excursion
     (let ((default-directory (counsel-gtags--default-directory)))
       (let* ((type 'definition)
	      (query "the_")
	      (query-command
	       (counsel-gtags--build-command-to-collect-candidates query '("--result=ctags")))
	      (collected-results
	       (s-split "\n" (shell-command-to-string query-command) t)))
	 (should
	  (equal collected-results
		 '("the_first_func"
		   "the_second_func"
		   "the_third_func"))))))))

(ert-deftest correct-no-color-option-for-ag ()
  "See https://github.com/FelipeLema/emacs-counsel-gtags/issues/11"
  (ert--skip-unless (executable-find "ag"))
  (let ((counsel-gtags--grep-commands '("ag" "grep" "rg")))
    (should
     (string-suffix-p "ag --nocolor" (counsel-gtags--get-grep-command)))))

(ert-deftest correct-no-color-option-for-grep ()
  "See https://github.com/FelipeLema/emacs-counsel-gtags/issues/11"
  (ert--skip-unless (executable-find "grep"))
  (let ((counsel-gtags--grep-commands '("grep" "rg" "ag")))
    (should
     (string-suffix-p "grep --color=never" (counsel-gtags--get-grep-command)))))

(ert-deftest correct-no-color-option-for-rg ()
  "See https://github.com/FelipeLema/emacs-counsel-gtags/issues/11"
  (ert--skip-unless (executable-find "rg"))
  (let ((counsel-gtags--grep-commands '("rg" "ag" "grep")))
    (should
     (string-suffix-p "rg --color=never" (counsel-gtags--get-grep-command)))))

(ert-deftest tags-infile ()
  (counsel-gtags--with-mock-project
   (save-window-excursion
     (with-current-buffer (find-file main-file-path)
       (let ((type 'tags)
	     (query main-file-path)
	     (query-command
	      (counsel-gtags--build-command-to-collect-candidates query '("--result=ctags")))
	     (collected-results
	      (s-split "\n" (shell-command-to-string query-command) t))))
       (should
        (equal collected-results
	       '("the_first_func"
		 "the_second_func"
		 "the_third_func")))))))


(provide 'unit-tests)
;;; unit-tests.el ends here
