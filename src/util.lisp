(in-package #:cl-user)

(defpackage openai-sdk/util
  (:use #:cl)
  (:import-from #:openai-sdk/interface #:b64encode)
  (:export #:merge-plist
           #:objectify
           #:underscore-plist-keys
           #:b64encode
           #:format-b64))

(in-package #:openai-sdk/util)

(defun valid-url-p (s)
  (quri:uri-p s))

(defun format-b64 (b64encoded-string mime-type)
  (format nil "data:~a;base64,~a" mime-type b64encoded-string))

(defmethod b64encode ((path pathname))
  (unless (uiop:file-exists-p path)
    (error "Path does not exist."))
  (let ((element-type '(unsigned-byte 8)))
    (with-output-to-string (sink)
      (with-open-file (source path :element-type element-type)
        (with-open-stream (b64-encoder (make-instance 'qbase64:encode-stream
                                                      :underlying-stream sink))
          (uiop:copy-stream-to-stream source b64-encoder :element-type element-type))))))

(defun %load-plist-from-hash-table (ht)
  (let ((plist (alexandria:hash-table-plist ht)))
    (loop for el in plist
          for i from 0 to (length plist)
          collect (if (= 0 (mod i 2))
                      (symbol-munger:underscores->keyword el)
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
  "This function converts HT hash-table to a plist which is applied as function
 arguments to the MAKE-CLASS-FUNCTION-SYMBOL. The HT keys are transformed from
 snake-case to conventional kebab-case symbols."
  (if (hash-table-p ht)
      (apply make-class-function-symbol
             (%load-plist-from-hash-table ht))
      ht))

(defun underscore-plist-keys (plist)
  (loop for el in plist
        for i from 0 to (length plist)
        collect (if (= 0 (mod i 2))
                    (symbol-munger:lisp->underscores el)
                    el)))
