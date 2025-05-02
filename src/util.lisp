(in-package #:cl-user)

(defpackage openai-sdk/util
  (:use #:cl)
  (:export #:merge-plist
           #:objectify
           #:underscore-plist-keys))

(in-package #:openai-sdk/util)


(defun load-plist-from-hash-table (ht)
  (let ((plist (alexandria:hash-table-plist ht)))
    (loop for el in plist
          for i from 0 to (length plist)
          collect (if (= 0 (mod i 2))
                      (symbol-munger:english->keyword el)
                      el))))

(defun merge-plist (default override)
  (loop with notfound = '#:notfound
        for (indicator value) on default by #'cddr
        when (eq (getf override indicator notfound) notfound)
          do (progn
               (push value override)
               (push indicator override)))
  override)


(defun objectify (make-class-function-symbol ht)
  (if (hash-table-p ht)
      (apply make-class-function-symbol
             (load-plist-from-hash-table ht))
      ht))


(defun underscore-plist-keys (plist)
  (loop for el in plist
        for i from 0 to (length plist)
        collect (if (= 0 (mod i 2))
                    (symbol-munger:lisp->underscores el)
                    el)))
