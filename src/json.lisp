(in-package #:cl-user)

(uiop:define-package openai-sdk/json
  (:use #:cl)
  (:nicknames #:oai/json)
  (:export #:schema
           #:parse
           #:def-schema-list-type
           ;; Predefined list types
           #:string-oai-list
           #:integer-oai-list
           #:number-oai-list
           #:boolean-oai-list))

(in-package #:openai-sdk/json)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *openai-list-type-suffix* "-OAI-LIST"))

(defmacro def-schema-list-type (class)
  (let ((class-string (format nil "~a" class)))
    (flet ((funsym (el str) (intern (concatenate 'string el str))))
      (let ((class-sym (funsym class-string "-LIST-P"))
            (type-sym (funsym class-string *openai-list-type-suffix*)))
        `(progn
           (defun ,class-sym (el)
             (and (consp el)
                  (typep el ',class)))
           (deftype ,type-sym ()
             '(satisfies ,class-sym))
           '(,class-sym ,type-sym))))))

(def-schema-list-type string)           ; string-oai-list
(def-schema-list-type integer)          ; integer-oai-list
(def-schema-list-type number)           ; number-oai-list
(def-schema-list-type boolean)          ; boolean-oai-list

(defvar *default-types*
  '((string . "string")
    (integer . "integer")
    (number . "number")
    (boolean . "boolean")
    (null . "null")))

(defun slot-names (class)
  (mapcar
   (lambda (slot) (symbol-munger:lisp->underscores (c2mop:slot-definition-name slot)))
   (c2mop:class-slots (c2mop:ensure-finalized (find-class class)))))

(defun generate-default-prop (slot-type)
  (alexandria:plist-hash-table (list "type"
                                     (cdr (assoc slot-type
                                                 *default-types*)))
                               :test #'equal))

(defun list-type-p (slot-type)
  (search *openai-list-type-suffix* (format nil "~A" slot-type) :test #'equal :from-end t))

(defun class-symbol (list-type-symbol)
  (let ((type-str (format nil "~A" list-type-symbol))
        (package-name (symbol-package list-type-symbol)))
    (intern (subseq type-str 0 (- (length type-str)
                                  (length *openai-list-type-suffix*)))
            (find-package package-name))))

(defun generate-object-type (class)
  (alexandria:alist-hash-table
   `((type . "object")
     (properties . ,(alexandria:alist-hash-table
                     (mapcar (lambda (slot)
                               (cons (symbol-munger:lisp->underscores (c2mop:slot-definition-name slot))
                                     (generate-property-value slot)))
                             (c2mop:class-slots (c2mop:ensure-finalized (find-class class))))))
     (required . ,(slot-names class))
     (|additionalProperties| . nil))))

(defun generate-property-value (slot)
  (let ((slot-type (c2mop:slot-definition-type slot)))
    (cond
      ((assoc slot-type *default-types*) (generate-default-prop slot-type))
      ((and (consp slot-type) (eq 'member (car slot-type)))
       (let ((items (mapcar 'symbol-munger:lisp->underscores
                            (cdr (introspect-environment:typexpand
                                  slot-type)))))
         (generate-default-prop (if (= 1 (length items))
                                    (car items)
                                    items))))
      ((find-class slot-type nil) (generate-object-type slot-type))
      ((and (eql 'satisfies (car (introspect-environment:typexpand slot-type)))
            (list-type-p slot-type))
       (alexandria:plist-hash-table
        (let* ((type-class (class-symbol slot-type))
               (default-type-p (assoc type-class *default-types*)))
          (list 'type "array"
                'items (or (and default-type-p (generate-default-prop (car default-type-p)))
                           (generate-object-type type-class)))))))))

(defun schema (class-symbol)
  (generate-object-type class-symbol))

(defun parse-hash-table (hash-table class)
  (make-instance 'class))

(defun parse (class-symbol hash-table)
  (declare (optimize (debug 3)))
  (let* ((object (make-instance class-symbol))
         (slots (c2mop:class-slots (class-of object))))
    (loop for key being the hash-key of hash-table
          for value being the hash-value of hash-table
          do (let ((slot (find-if (lambda (slot)
                                    (let ((slot-name (c2mop:slot-definition-name slot)))
                                      (and (equal key (symbol-munger:lisp->underscores
                                                       slot-name))
                                           (not (slot-boundp object slot-name)))))
                                  slots)))
               (setf (slot-value object (c2mop:slot-definition-name slot))
                     (typecase value
                       (simple-vector (if (notevery (lambda (val)
                                                      (loop for el in (mapcar #'car *default-types*)
                                                            thereis (typep val el)))
                                                    value)
                                          (map 'simple-vector
                                               (lambda (item)
                                                 (parse (find-class (class-symbol (c2cl:slot-definition-type slot)))
                                                        item))
                                               value)
                                          value))
                       (hash-table (parse (find-class (c2mop:slot-definition-type slot))
                                          value))
                       (t value)))))
    object))
